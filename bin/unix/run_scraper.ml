let map_error f = function Ok _ as r -> r | Error e -> Error (f e)

let read_scraper () =
  match Sexplib.Sexp.input_sexps stdin with
  | exception Sexplib.Sexp.Parse_error { err_msg; _ } ->
      Error ("Syntax error: " ^ err_msg)
  | sexp -> (
      try Ok (Feeds_config.parse_scraper sexp) with Failure e -> Error e
    )

let ( >>= ) x f =
  Lwt.bind x (function Ok x -> f x | Error _ as e -> Lwt.return e)

let run uri =
  Lwt.return (read_scraper ()) >>= fun scraper ->
  let resolve_uri = Uri.resolve "" uri in
  Fetch.fetch uri |> Lwt.map (map_error (Format.asprintf "%a" Fetch.pp_error))
  >>= fun contents ->
  let feed = Scraper.scrap ~resolve_uri scraper contents in
  Print.print_feed feed;
  Lwt.return (Ok ())
