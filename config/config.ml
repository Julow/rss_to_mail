open Sexplib0.Sexp

let record field = function
  | List ts ->
      List.find_map
        (function
          | List (Atom f :: t) when String.equal f field -> (
              match t with [ t ] -> Some t | ts -> Some (List ts)
            )
          | _ -> None)
        ts
  | Atom _ -> failwith "Expecting record"

let atom = function List _ -> failwith "Expecting string" | Atom s -> s

let list f = function List ts -> f ts | Atom _ as t -> f [ t ]

let one = function [ v ] -> v | _ -> failwith "Expecting a single value"

let one_or_more = function [] -> failwith "Expecting a value" | v -> v

let parse_scraper =
  let open Scrap in
  let rec scraper ~target = function
    | List (Atom "R" :: rs) -> R (List.map (rule ~target) rs)
    | List (Atom "T" :: ts) -> T (List.map target ts)
    | _ -> failwith "Malformated scraper"
  and rule ~target = function
    | List [ Atom sel; t ] -> (sel, scraper ~target t)
    | _ -> failwith "Malformated scraper rule"
  in
  let open Scraper in
  let target = function
    | Atom "feed_title" -> Feed_title
    | Atom "feed_icon" -> Feed_icon
    | List (Atom "entry" :: ts) ->
        let target = function
          | Atom "id" -> Id
          | Atom "title" -> Title
          | Atom "link" -> Link
          | Atom "summary" -> Summary
          | Atom "thumbnail" -> Thumbnail
          | Atom "attachment" -> Attachment { attach_type = None }
          | List [ Atom "attachment"; Atom attach_type ] ->
              Attachment { attach_type = Some attach_type }
          | _ -> failwith "Invalid target"
        in
        Entry (List.map (scraper ~target) ts)
    | Atom target -> failwith ("Invalid target: " ^ target)
    | List _ -> failwith "Expecting target"
  in
  fun ts -> R (List.map (rule ~target) ts)

let parse_filter = function
  | List [ Atom r ] | Atom r -> (Str.regexp r, true)
  | List (Atom "not" :: values) -> (Str.regexp (atom (one values)), false)
  | _ -> failwith "Malformated"

let check_duplicate feeds =
  let tbl = Hashtbl.create (List.length feeds) in
  let check_url url =
    if Hashtbl.mem tbl url then failwith ("Feed declared twice: " ^ url);
    Hashtbl.add tbl url ()
  in
  List.iter (fun (feed, _) -> check_url (Feed_desc.url_of_feed feed)) feeds

type t = {
  server : string * int;
  server_auth : [ `Plain of string * string ];
  from_address : string;
  to_address : string;
  feeds : Feed_desc.t list;
}

let parse sexp =
  let parse_option_refresh =
    let parse_time time =
      match Scanf.sscanf time "%d:%d" (fun h m -> (h, m)) with
      | exception _ -> failwith "Malformated"
      | h, m when h < 0 || h > 23 || m < 0 || m > 59 -> failwith "Invalid time"
      | t -> t
    and parse_day = function
      | "mon" -> CalendarLib.Date.Mon
      | "tue" -> Tue
      | "wed" -> Wed
      | "thu" -> Thu
      | "fri" -> Fri
      | "sat" -> Sat
      | "sun" -> Sun
      | _ -> failwith "Invalid day"
    in
    function
    | Atom hours -> `Every (float_of_string hours)
    | List [ Atom "at"; Atom time ] -> `At (parse_time time)
    | List [ Atom "at"; Atom time; Atom day ] ->
        let h, m = parse_time time and d = parse_day day in
        `At_weekly (d, h, m)
    | List _ -> failwith "Malformated"
  in

  let parse_option name values (opts : Feed_desc.options) =
    match name with
    | "refresh" -> { opts with refresh = parse_option_refresh (one values) }
    | "title" -> { opts with title = Some (atom (one values)) }
    | "label" -> { opts with label = Some (atom (one values)) }
    | "no_content" ->
        { opts with no_content = bool_of_string (atom (one values)) }
    | "filter" -> { opts with filter = List.map parse_filter values }
    | "to" -> { opts with to_ = Some (atom (one values)) }
    | _ -> failwith "Unknown option"
  in

  let rec parse_options opts = function
    | List (Atom name :: values) :: tl -> (
        match parse_option name values opts with
        | exception Failure msg -> failwith ("\"" ^ name ^ "\": " ^ msg)
        | opts -> parse_options opts tl
      )
    | _ :: _ -> failwith "Malformated options"
    | [] -> opts
  in

  let parse_feed ~default_opts =
    let parse_options ~url options =
      match parse_options default_opts options with
      | exception Failure msg -> failwith (url ^ ": " ^ msg)
      | options -> options
    in
    let open Feed_desc in
    let rec parse_desc = function
      | Atom url -> Feed url
      | List (Atom "scraper" :: url :: scraper) ->
          let url = atom url and scraper = one_or_more scraper in
          let scraper =
            try parse_scraper scraper
            with Failure msg -> failwith (url ^ ": " ^ msg)
          in
          Scraper (url, scraper)
      | List (Atom "bundle" :: desc) -> (
          match parse_desc (one desc) with
          | (Feed _ | Scraper _) as in_bundle -> Bundle in_bundle
          | Bundle _ -> failwith "Bundle in bundle"
        )
      | List (Atom kind :: _) -> failwith (kind ^ ": Unknown feed kind")
      | List _ -> failwith "feeds: Syntax error"
    in
    function
    | Atom url -> (Feed url, default_opts)
    | List (desc :: opts) ->
        let desc = parse_desc desc in
        let opts = parse_options ~url:(Feed_desc.url_of_feed desc) opts in
        (desc, opts)
    | List [] -> failwith "feeds: Syntax error"
  in

  let parse_feeds ~default_opts t =
    let parse acc = function
      | List (Atom "with-options" :: List opts :: feeds) ->
          let default_opts = parse_options default_opts opts in
          List.rev_map (parse_feed ~default_opts) feeds @ acc
      | feed -> parse_feed ~default_opts feed :: acc
    in
    list (List.fold_left parse []) t |> List.rev
  in

  let parse_smtp t =
    let server =
      match record "server" t with
      | Some (List [ Atom host ]) | Some (Atom host) -> (host, 465)
      | Some (List [ Atom host; Atom port ]) -> (host, int_of_string port)
      | Some _ -> failwith "Malformated field `server`"
      | None -> failwith "Missing field `server`"
    in
    let from =
      match record "from" t with
      | Some (Atom from) -> from
      | Some _ -> failwith "Malformated field `from`"
      | None -> failwith "Missing field `from`"
    in
    let auth =
      match record "auth" t with
      | Some (List [ Atom user; Atom pass ]) -> `Plain (user, pass)
      | None -> failwith "Missing field `auth`"
      | Some _ -> failwith "Malformated field `auth`"
    in
    (server, auth, from)
  in
  let default_opts =
    let refresh =
      try Option.map parse_option_refresh (record "default_refresh" sexp)
      with Failure msg -> failwith ("default_refresh: " ^ msg)
    in
    Feed_desc.make_options ?refresh ()
  in
  let feeds =
    match record "feeds" sexp with
    | Some t -> parse_feeds ~default_opts t
    | None -> failwith "Missing field `feeds`"
  and server, server_auth, from_address =
    match record "smtp" sexp with
    | Some t -> parse_smtp t
    | None -> failwith "Missing field `smtp`"
  and to_address =
    match record "to" sexp with
    | Some (Atom a) -> a
    | Some _ -> failwith "Malformated field `to`"
    | None -> failwith "Missing field `to`"
  in
  check_duplicate feeds;
  { server; server_auth; from_address; to_address; feeds }
