open Printf

(* Fetch feeds in the `feeds/` subdirectory
   Fetchs are blocking *)
module Local_fetch = struct
  type error = int

  let fetch uri =
    (* Take path, ignore the rest of URI *)
    let f = "feeds/" ^ Uri.path uri in
    eprintf "opening %s\n" f;
    Lwt.return
    @@
    match open_in f with
    | exception Sys_error _ -> Error 404
    | inp ->
        let len = in_channel_length inp in
        Ok (really_input_string inp len)
end

module Feed_datas = struct
  module StringMap = Map.Make (String)

  type t = Rss_to_mail.feed_data StringMap.t

  let get t url = StringMap.find_opt url t

  let set t url data = StringMap.add url data t
end

module Rss_to_mail = Rss_to_mail.Make (Local_fetch) (Feed_datas)

let now = 12345678L

let print_mail (m : Rss_to_mail.mail) =
  printf "FROM: %s\nSUBJECT: %s\nBODY html: %s\nBODY text: %s\n" m.sender
    m.subject m.body_html m.body_text

let print_options (opts : Feed_desc.options) =
  printf "Options:";
  ( match opts.refresh with
  | `Every h -> printf " (refresh %f)" h
  | `At (h, m) -> printf " (refresh (at %d:%d))" h m
  | `At_weekly (d, h, m) ->
      printf " (refresh (at %d:%d %d)" h m (CalendarLib.Date.int_of_day d)
  );
  Option.iter (printf " (title %s)") opts.title;
  Option.iter (printf " (label %s)") opts.label;
  if opts.no_content then printf " (no_content true)";
  printf "\n"

let print_feed (feed, options) =
  ( match feed with
  | Feed_desc.Feed url -> printf "\n# %s\n\n" url
  | Scraper (url, _) -> printf "\n# scraper %s\n\n" url
  | Bundle (Feed url) -> printf "\n# bundle %s\n\n" url
  | Bundle (Scraper (url, _)) -> printf "\n# bundle scraper %s\n\n" url
  );
  print_options options

let print_log = function
  | url, `Fetch_error code -> printf "Log: %s: Fetch error %d\n" url code
  | url, `Parsing_error ((line, col), msg) ->
      printf "Log: %s: Parsing error (line %d, col %d)\n%s\n" url line col msg
  | url, `Updated { Rss_to_mail.entries } ->
      printf "Log: %s: %d entries\n" url entries
  | url, `Uptodate -> printf "Log: %s: Uptodate\n" url

let () =
  let { Persistent_data.feed_datas; _ } =
    let sexp = Sexplib.Sexp.load_sexps "feed_datas.sexp" in
    Persistent_data.load sexp
  in
  let { Config.feeds; _ } =
    let sexp = Sexplib.Sexp.load_sexp "feeds.sexp" in
    Config.parse sexp
  in
  List.iter print_feed feeds;
  printf "\n# Done parsing\n\n";
  let _, mails, logs =
    Rss_to_mail.check_all ~now feed_datas feeds |> Lwt_main.run
  in
  List.iter print_log logs;
  List.iter print_mail (List.rev mails)
