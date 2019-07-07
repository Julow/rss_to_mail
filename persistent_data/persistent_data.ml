(** Parser for the `feeds.sexp` config file
    	and serializer/deserializer for the `feed_datas.sexp` persistent file
    	Used by main_native *)

open Std

type sexp = [ `Atom of string | `List of sexp list ]

let record field =
  function
  | `List ts	->
    List.find_map (function
        | `List (`Atom f :: t) when String.equal f field ->
          begin match t with
            | [ t ] -> Some t
            | ts -> Some (`List ts)
          end
        | _ -> None) ts
  | `Atom _	-> failwith "Expecting record"

let atom =
  function
  | `List _	-> failwith "Expecting string"
  | `Atom s	-> s

let list f =
  function
  | `List ts		-> f ts
  | `Atom _ as t	-> f [ t ]

let one =
  function
  | [ v ]		-> v
  | _			-> failwith "Expecting a single value"

let parse_scraper =
  let open Scrap in
  let rec scraper ~target =
    function
    | `List (`Atom "R" :: rs)	-> R (List.map (rule ~target) rs)
    | `List (`Atom "T" :: ts)	-> T (List.map target ts)
    | _							-> failwith "Malformated scraper"
  and rule ~target =
    function
    | `List [ `Atom sel; t ]	-> sel, scraper ~target t
    | _							-> failwith "Malformated scraper rule"
  in
  let open Scraper in
  let target =
    function
    | `Atom "feed_title"			-> Feed_title
    | `Atom "feed_icon"				-> Feed_icon
    | `List (`Atom "entry" :: ts)	->
      let target =
        function
        | `Atom "title"		-> Title
        | `Atom "link"		-> Link
        | `Atom "summary" -> Summary
        | _					-> failwith "Invalid target"
      in
      Entry (List.map (scraper ~target) ts)
    | _								-> failwith "Invalid target"
  in
  fun t -> R [ rule ~target t ]

let parse_filter =
  function
  | `List [ `Atom r ] | `Atom r	-> (Str.regexp r, true)
  | `List (`Atom "not" :: values)	-> (Str.regexp (atom (one values)), false)
  | _ -> failwith "Malformated"

let check_duplicate feeds =
  let module StringTbl = Hashtbl.Make (String) in
  let tbl = StringTbl.create (List.length feeds) in
  let check_url url =
    if StringTbl.mem tbl url then
      failwith ("Feed declared twice: " ^ url);
    StringTbl.add tbl url ()
  in
  feeds |> List.iter Feed_desc.(function
      | Feed url, _			-> check_url url
      | Scraper (url, _), _	-> check_url url
      | Bundle url, _			-> check_url url
    )

type config = {
  server : string * int;
  server_auth : [ `Plain of string * string ];
  from_address : string;
  to_address : string;
  feeds	: Feed_desc.t list
}

let load_feeds (sexp : sexp) =
  let parse_option_refresh =
    let parse_time time =
      match Scanf.sscanf time "%d:%d" (fun h m -> h, m) with
      | exception _ -> failwith "Malformated"
      | h, m when h < 0 || h > 23 || m < 0 || m > 59 ->
        failwith "Invalid time"
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
    | `Atom hours	-> `Every (float_of_string hours)
    | `List [ `Atom "at"; `Atom time ] -> `At (parse_time time)
    | `List [ `Atom "at"; `Atom time; `Atom day ] ->
      let h, m = parse_time time and d = parse_day day in
      `At_weekly (d, h, m)
    | `List _		-> failwith "Malformated"
  in

  let parse_option name values (opts : Feed_desc.options) =
    match name with
    | "refresh"		->
      { opts with refresh = parse_option_refresh (one values) }
    | "title"		-> { opts with title = Some (atom (one values)) }
    | "label"		-> { opts with label = Some (atom (one values)) }
    | "no_content"	->
      { opts with no_content = bool_of_string (atom (one values)) }
    | "filter"		-> { opts with filter = List.map parse_filter values }
    | _				-> failwith "Unknown option"
  in

  let rec parse_options opts =
    function
    | `List (`Atom name :: values) :: tl ->
      begin match parse_option name values opts with
        | exception (Failure msg)	->
          failwith ("\"" ^ name ^ "\": " ^ msg)
        | opts					-> parse_options opts tl
      end
    | _ :: _	-> failwith "Malformated options"
    | []		-> opts
  in

  let parse_feed ~default_opts =
    let parse_options ~url options =
      match parse_options default_opts options with
      | exception Failure msg	-> failwith (url ^ ": " ^ msg)
      | options				-> options
    in
    let open Feed_desc in
    function
    | `Atom url					->
      Feed url, default_opts
    | `List ((`List (`Atom "scraper" :: url :: scraper)) :: opts) ->
      let url = atom url and scraper = one scraper in
      let scraper = try parse_scraper scraper
        with Failure msg -> failwith (url ^ ": " ^ msg) in
      Scraper (url, scraper), parse_options ~url opts
    | `List ((`List (`Atom "bundle" :: url)) :: opts) ->
      let url = atom (one url) in
      Bundle url, parse_options ~url opts
    | `List (`Atom url :: opts)	->
      Feed url, parse_options ~url opts
    | _ -> failwith "feeds: Syntax error"
  in

  let parse_smtp t =
    let server =
      match record "server" t with
      | Some (`List [ `Atom host ]) | Some (`Atom host) -> host, 465
      | Some (`List [ `Atom host; `Atom port ]) -> host, int_of_string port
      | Some _ -> failwith "Malformated field `server`"
      | None -> failwith "Missing field `server`"
    in
    let from =
      match record "from" t with
      | Some (`Atom from) -> from
      | Some _ -> failwith "Malformated field `from`"
      | None -> failwith "Missing field `from`"
    in
    let auth =
      match record "auth" t with
      | Some (`List [ `Atom user; `Atom pass ]) -> `Plain (user, pass)
      | None -> failwith "Missing field `auth`"
      | Some _ -> failwith "Malformated field `auth`"
    in
    server, auth, from
  in
  let default_opts =
    let refresh =
      try Option.map parse_option_refresh (record "default_refresh" sexp)
      with Failure msg -> failwith ("default_refresh: " ^ msg)
    in
    Feed_desc.make_options ?refresh ()
  in
  let feeds = match record "feeds" sexp with
    | Some t	-> list (List.map (parse_feed ~default_opts)) t
    | None		-> failwith "Missing field `feeds`"
  and server, server_auth, from_address = match record "smtp" sexp with
    | Some t	-> parse_smtp t
    | None		-> failwith "Missing field `smtp`"
  and to_address = match record "to" sexp with
    | Some (`Atom a)	-> a
    | Some _			-> failwith "Malformated field `to`"
    | None				-> failwith "Missing field `to`"
  in
  check_duplicate feeds;
  { server; server_auth; from_address; to_address; feeds }

open Sexplib.Std

module Feed_datas =
struct

  module Raw =
  struct

    type unsent_mail = Rss_to_mail.mail = {
      sender : string;
      subject : string;
      body_html : string;
      body_text : string;
    }
    [@@deriving sexp]

    type seenset = (string * int64 option) list
    [@@deriving sexp]

    type feed_data = string * int64 * seenset
    [@@deriving sexp]

    type t = {
      feed_data : feed_data list;
      unsent_mails : unsent_mail list
    }
    [@@deriving sexp]

    let parse_file name =
      match t_of_sexp (Sexplib.Sexp.load_sexp name) with
      | exception Failure msg -> Error msg
      | exception Sexplib0.Sexp_conv.Of_sexp_error (Failure msg, _) -> Error msg
      | exception Sexplib.Sexp.Parse_error { err_msg; _ } -> Error err_msg
      | t -> Ok t

    let save_to_file name t =
      Sexplib.Sexp.save_mach name (sexp_of_t t)

  end

  module StringMap = Map.Make (String)

  type t = {
    feed_datas : (int64 * SeenSet.t) StringMap.t;
    unsent_mails : Rss_to_mail.mail list;
  }

  let empty = { feed_datas = StringMap.empty; unsent_mails = [] }

  let gen_seenset acc = function
    | id, None -> SeenSet.add id acc
    | id, Some tm -> SeenSet.remove tm id acc

  let gen_datas acc (url, tm, seens) =
    let seens = List.fold_left gen_seenset SeenSet.empty seens in
    StringMap.add url (tm, seens) acc

  let ungen_seenset id rem acc =
    (id, rem) :: acc

  let ungen_datas url (tm, seens) acc =
    (url, tm, SeenSet.fold ungen_seenset seens []) :: acc

  let parse_file name =
    match Raw.parse_file name with
    | Error _ as err -> err
    | Ok Raw.{ feed_data; unsent_mails } ->
      let feed_datas = List.fold_left gen_datas StringMap.empty feed_data in
      Ok { feed_datas; unsent_mails }

  let save_to_file name { feed_datas; unsent_mails } =
    let feed_data = StringMap.fold ungen_datas feed_datas [] in
    Raw.save_to_file name Raw.{ feed_data; unsent_mails }

end
