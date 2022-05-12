open Sexplib0.Sexp
open Error

let spf = Format.asprintf

let record field = function
  | List ts ->
      List.find_map
        (function
          | List (Atom f :: t) when String.equal f field -> (
              match t with [ t ] -> Some t | ts -> Some (List ts)
            )
          | _ -> None
          )
        ts
  | Atom _ -> raise_error "Expected list"

let atom = function List _ -> raise_error "Expected string" | Atom s -> s
let list = function List ts -> ts | Atom _ as t -> [ t ]
let one = function [ v ] -> v | _ -> raise_error "Expected a single value"
let one_or_more = function [] -> raise_error "Expected a value" | v -> v

let parse_scraper =
  let open Scrap in
  let rec scraper ~target = function
    | List (Atom "R" :: rs) -> R (List.map (rule ~target) rs)
    | List (Atom "T" :: ts) -> T (List.map target ts)
    | _ -> raise_error "Expected '(R ...)' or '(T ...)'"
  and rule ~target = function
    | List [ Atom sel; t ] -> (sel, scraper ~target t)
    | _ -> raise_error "Expected scraper rule"
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
          | _ -> raise_error "Invalid target"
        in
        Entry (List.map (scraper ~target) ts)
    | Atom target -> raise_error ("Invalid target: " ^ target)
    | List _ -> raise_error "Expected target"
  in
  fun ts -> R (List.map (rule ~target) ts)

let rec parse_filter_expr =
  let open Feed_desc in
  function
  | List (Atom "and" :: fs) -> And (List.map parse_filter_expr fs)
  | List (Atom "or" :: fs) -> Or (List.map parse_filter_expr fs)
  | List (Atom "not" :: ts) -> Not (parse_filter_expr (one ts))
  | List (Atom "title" :: ts) -> Match_title (Str.regexp (atom (one ts)))
  | List (Atom "content" :: ts) -> Match_content (Str.regexp (atom (one ts)))
  | List (Atom op :: _) -> raise_error (spf "Invalid filter operator %S" op)
  | List _ | Atom _ -> raise_error "Invalid filter expression"

let check_duplicate feeds =
  let tbl = Hashtbl.create (List.length feeds) in
  let check_url url =
    if Hashtbl.mem tbl url then raise_error ("Feed declared twice: " ^ url);
    Hashtbl.add tbl url ()
  in
  List.iter
    (fun (feed, _) ->
      let ((#Feed_desc.regular_feed as desc) | `Bundle desc) = feed in
      check_url (Feed_desc.url_of_feed desc)
    )
    feeds

type t = {
  server : string * int;
  server_auth : [ `Plain of string * string ];
  from_address : string;
  to_address : string;
  feeds : Feed_desc.t list;
}

let parse_option_refresh =
  let parse_time time =
    match Scanf.sscanf time "%d:%d" (fun h m -> (h, m)) with
    | exception _ -> raise_error "Invalid time value"
    | h, m when h < 0 || h > 23 || m < 0 || m > 59 ->
        raise_error "Invalid time value"
    | t -> t
  and parse_day = function
    | "mon" -> `Mon
    | "tue" -> `Tue
    | "wed" -> `Wed
    | "thu" -> `Thu
    | "fri" -> `Fri
    | "sat" -> `Sat
    | "sun" -> `Sun
    | _ -> raise_error "Invalid day"
  in
  function
  | Atom hours -> `Every (float_of_string hours)
  | List [ Atom "at"; Atom time ] -> `At (parse_time time)
  | List [ Atom "at"; Atom time; Atom day ] ->
      let h, m = parse_time time and d = parse_day day in
      `At_weekly (d, h, m)
  | List _ -> raise_error "Invalid value"

and parse_option_max_entries t =
  let s = atom t in
  try int_of_string s with _ -> raise_error (spf "Expected integer, got %S" s)

let parse_options =
  let parse_content_option t =
    match atom (one t) with
    | "keep" -> `Keep
    | "remove" -> `Remove
    | _ -> raise_error "Invalid value"
  in

  let parse_option name values (opts : Feed_desc.options) =
    match name with
    | "refresh" -> { opts with refresh = parse_option_refresh (one values) }
    | "title" -> { opts with title = Some (atom (one values)) }
    | "label" -> { opts with label = Some (atom (one values)) }
    | "content" -> { opts with content = parse_content_option values }
    | "filter" -> { opts with filter = Some (parse_filter_expr (one values)) }
    | "to" -> { opts with to_ = Some (atom (one values)) }
    | "max_entries" ->
        { opts with max_entries = Some (parse_option_max_entries (one values)) }
    | _ -> raise_error (spf "Unknown option %S" name)
  in

  List.fold_left (fun opts t ->
      match list t with
      | Atom name :: values ->
          let@ () = with_context (fun () -> spf "option %S" name) in
          parse_option name values opts
      | _ -> raise_error "Expected option"
  )

let parse_feed ~default_opts =
  let open Feed_desc in
  let rec parse_desc = function
    | Atom url -> `Feed url
    | List (Atom "scraper" :: url :: scraper) ->
        let url = atom url and scraper = one_or_more scraper in
        let@ () = with_context (fun () -> spf "Scraper %S" url) in
        `Scraper (url, parse_scraper scraper)
    | List (Atom "bundle" :: desc) -> (
        match parse_desc (one desc) with
        | #regular_feed as desc -> `Bundle desc
        | `Bundle _ -> raise_error "Bundle in bundle"
      )
    | List (Atom kind :: _) -> raise_error (kind ^ ": Unknown feed kind")
    | List _ -> raise_error "feeds: Syntax error"
  in
  function
  | Atom url -> (`Feed url, default_opts)
  | List (desc :: opts) ->
      let desc = parse_desc desc in
      let context_url =
        match desc with
        | (#regular_feed as desc) | `Bundle desc -> Feed_desc.url_of_feed desc
      in
      let@ () = with_context (fun () -> spf "Feed %S" context_url) in
      (desc, parse_options default_opts opts)
  | List [] -> raise_error "feeds: Syntax error"

let parse sexp =
  let parse_feeds ~default_opts t =
    let parse acc = function
      | List (Atom "with-options" :: List opts :: feeds) ->
          let default_opts = parse_options default_opts opts in
          List.rev_map (parse_feed ~default_opts) feeds @ acc
      | feed -> parse_feed ~default_opts feed :: acc
    in
    List.fold_left parse [] (list t) |> List.rev
  in

  let parse_default_opts t =
    let refresh =
      Option.map parse_option_refresh (record "default_refresh" t)
    in
    let max_entries =
      Option.map parse_option_max_entries (record "max_entries" t)
    in
    Feed_desc.make_options ?refresh ?max_entries ()
  in

  let parse_smtp t =
    let server =
      match record "server" t with
      | Some (List [ Atom host ]) | Some (Atom host) -> (host, 465)
      | Some (List [ Atom host; Atom port ]) -> (host, int_of_string port)
      | Some _ -> raise_error "Malformated field `server`"
      | None -> raise_error "Missing field `server`"
    in
    let from =
      match record "from" t with
      | Some (Atom from) -> from
      | Some _ -> raise_error "Malformated field `from`"
      | None -> raise_error "Missing field `from`"
    in
    let auth =
      match record "auth" t with
      | Some (List [ Atom user; Atom pass ]) -> `Plain (user, pass)
      | None -> raise_error "Missing field `auth`"
      | Some _ -> raise_error "Malformated field `auth`"
    in
    (server, auth, from)
  in
  let parse t =
    let default_opts = parse_default_opts t in
    let feeds =
      match record "feeds" t with
      | Some t -> parse_feeds ~default_opts t
      | None -> raise_error "Missing field `feeds`"
    and server, server_auth, from_address =
      match record "smtp" t with
      | Some t -> parse_smtp t
      | None -> raise_error "Missing field `smtp`"
    and to_address =
      match record "to" t with
      | Some (Atom a) -> a
      | Some _ -> raise_error "Malformated field `to`"
      | None -> raise_error "Missing field `to`"
    in
    check_duplicate feeds;
    { server; server_auth; from_address; to_address; feeds }
  in
  handle_error (fun () -> parse sexp)

let parse_scraper sexps = handle_error (fun () -> parse_scraper sexps)

let parse_feed ~default_opts sexp =
  handle_error (fun () -> parse_feed ~default_opts sexp)
