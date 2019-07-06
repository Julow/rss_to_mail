let map_error f = function Ok _ as r -> r | Error e -> Error (f e)

let http_uri uri =
  let uri = Uri.of_string uri in
  match Uri.scheme uri with
  | Some "http" | Some "https" -> Some uri
  | Some _ | None -> None

let fetch src =
  match http_uri src with
  | Some src -> Fetch.fetch src |> Lwt.map (map_error Fetch.error_to_string)
  | None -> Lwt.return (CCIO.File.read src)

let read_scraper () =
  match CCSexp.parse_chan stdin with
  | Ok sexp -> (
      try Ok (Persistent_data.parse_scraper sexp)
      with Failure e -> Error e
    )
  | Error msg -> Error ("Syntax error: " ^ msg)

let print_feed (feed : Feed.t) =
  let opt = function Some s -> s | None -> "None" in
  let p = Printf.printf in
  let print_entry (t : Feed.entry) =
    p "\tid: %s\n" (opt t.id);
    p "\ttitle: %s\n" (opt t.title);
    p "\tlink: %s\n" (opt (Option.map Uri.to_string t.link));
  in
  p "Feed title: %s\n" (opt feed.feed_title);
  p "Feed link: %s\n" (opt (Option.map Uri.to_string feed.feed_link));
  p "Feed icon: %s\n" (opt (Option.map Uri.to_string feed.feed_icon));
  p "Entries (%d):\n" (Array.length feed.entries);
  Array.iter print_entry feed.entries

let (>>=) x f = Lwt.bind x (function Ok x -> f x | Error _ as e -> Lwt.return e)

let run src =
  Lwt.return (read_scraper ()) >>= fun scraper ->
  fetch src >>= fun contents ->
  let feed = Scraper.scrap scraper contents in
  print_feed feed;
  Lwt.return (Ok ())
