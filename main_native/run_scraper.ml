let map_error f = function Ok _ as r -> r | Error e -> Error (f e)

let http_uri uri =
  let uri = Uri.of_string uri in
  match Uri.scheme uri with
  | Some "http" | Some "https" -> Some uri
  | Some _ | None -> None

let source_kind src =
  match http_uri src with
  | Some uri ->
      let resolve_uri = Uri.resolve "" uri
      and fetch () =
        Fetch.fetch uri |> Lwt.map (map_error Fetch.error_to_string)
      in
      (resolve_uri, fetch)
  | None ->
      let resolve_uri u = u
      and fetch () =
        Lwt.catch
          (fun () ->
            Lwt_io.lines_of_file src
            |> Lwt_stream.to_list
            |> Lwt.map (fun lines -> Ok (String.concat "\n" lines))
          )
          (fun _ -> Lwt.return_error "Failed to read file")
      in
      (resolve_uri, fetch)

let read_scraper () =
  match Sexplib.Sexp.input_sexps stdin with
  | exception Sexplib.Sexp.Parse_error { err_msg; _ } ->
      Error ("Syntax error: " ^ err_msg)
  | sexp -> (
      try Ok (Feeds_config.parse_scraper sexp) with Failure e -> Error e
    )

let print_feed (feed : Feed.t) =
  let opt f = function Some s -> f s | None -> "None" in
  let list f = function
    | [] -> "None"
    | lst -> String.concat ", " (List.map f lst)
  in
  let attach_to_string Feed.{ attach_url; attach_type; _ } =
    Printf.sprintf "%s (type = %s)" (Uri.to_string attach_url)
      (opt Fun.id attach_type)
  in
  let p = Printf.printf in
  let print_entry (t : Feed.entry) =
    p "\tid: %s\n" (opt Fun.id t.id);
    p "\ttitle: %s\n" (opt Fun.id t.title);
    p "\tlink: %s\n" (opt Uri.to_string t.link);
    p "\tsummary: %s\n" (opt (fun c -> c.Feed.content_text) t.summary);
    p "\tthumbnail: %s\n" (opt Uri.to_string t.thumbnail);
    p "\tattachments: %s\n" (list attach_to_string t.attachments);
    ()
  in
  p "Feed title: %s\n" (opt Fun.id feed.feed_title);
  p "Feed link: %s\n" (opt Uri.to_string feed.feed_link);
  p "Feed icon: %s\n" (opt Uri.to_string feed.feed_icon);
  p "Entries (%d):\n" (Array.length feed.entries);
  Array.iter print_entry feed.entries

let ( >>= ) x f =
  Lwt.bind x (function Ok x -> f x | Error _ as e -> Lwt.return e)

let run src =
  Lwt.return (read_scraper ()) >>= fun scraper ->
  let resolve_uri, fetch = source_kind src in
  fetch () >>= fun contents ->
  let feed = Scraper.scrap ~resolve_uri scraper contents in
  print_feed feed;
  Lwt.return (Ok ())
