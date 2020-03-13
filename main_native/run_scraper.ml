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
    and fetch () = Fetch.fetch uri |> Lwt.map (map_error Fetch.error_to_string) in
    resolve_uri, fetch
  | None ->
    let resolve_uri u = u
    and fetch () = Lwt.return (CCIO.File.read src) in
    resolve_uri, fetch

let read_scraper () =
  match CCSexp.parse_chan_list stdin with
  | Ok sexp -> (
      try Ok (Persistent_data.parse_scraper sexp)
      with Failure e -> Error e
    )
  | Error msg -> Error ("Syntax error: " ^ msg)

let print_feed (feed : Feed.t) =
  let content_to_string = function Feed.Text s -> s | Html _ -> "<html>" in
  let opt f = function Some s -> f s | None -> "None" in
  let p = Printf.printf in
  let print_entry (t : Feed.entry) =
    p "\tid: %s\n" (opt id t.id);
    p "\ttitle: %s\n" (opt id t.title);
    p "\tlink: %s\n" (opt Uri.to_string t.link);
    p "\tsummary: %s\n" (opt content_to_string t.summary);
  in
  p "Feed title: %s\n" (opt id feed.feed_title);
  p "Feed link: %s\n" (opt Uri.to_string feed.feed_link);
  p "Feed icon: %s\n" (opt Uri.to_string feed.feed_icon);
  p "Entries (%d):\n" (Array.length feed.entries);
  Array.iter print_entry feed.entries

let (>>=) x f = Lwt.bind x (function Ok x -> f x | Error _ as e -> Lwt.return e)

let run src =
  Lwt.return (read_scraper ()) >>= fun scraper ->
  let resolve_uri, fetch = source_kind src in
  fetch () >>= fun contents ->
  let feed = Scraper.scrap ~resolve_uri scraper contents in
  print_feed feed;
  Lwt.return (Ok ())
