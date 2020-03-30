module StringMap = Map.Make (String)
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

let required = function Some v -> v | None -> failwith "Value required"

let opt f = function
  | List [] -> None
  | List [ t ] -> Some (f t)
  | Atom _ as t -> Some (f t)
  | List (_ :: _ :: _) -> failwith "Expecting a single value"

type t = {
  feed_datas : (int64 * SeenSet.t) StringMap.t;
  unsent_mails : Rss_to_mail.mail list;
}

let empty = { feed_datas = StringMap.empty; unsent_mails = [] }

let load sexp =
  let parse_ids set = function
    | List [ Atom id; Atom date ] ->
        SeenSet.remove (Int64.of_string date) id set
    | Atom id -> SeenSet.add id set
    | List _ -> failwith "load_feed_datas: parse_ids"
  in
  let parse_data m = function
    | List [ Atom url; Atom date; List ids ] ->
        let ids = List.fold_left parse_ids SeenSet.empty ids in
        StringMap.add url (Int64.of_string date, ids) m
    | _ -> failwith "load_feed_datas: parse_data"
  in
  let parse_unsent = function
    (* Compat, can be removed anytime *)
    | List [ Atom sender; Atom subject; Atom body_html ] ->
        Rss_to_mail.{ sender; subject; body_html; body_text = ""; to_ = None }
    | List [ Atom sender; Atom subject; Atom body_html; Atom body_text ] ->
        Rss_to_mail.{ sender; subject; body_html; body_text; to_ = None }
    | sexp ->
        let sender = atom @@ required @@ record "sender" sexp
        and to_ = Option.bind (record "to" sexp) (fun t -> opt atom t)
        and subject = atom @@ required @@ record "subject" sexp
        and body_html = atom @@ required @@ record "body_html" sexp
        and body_text = atom @@ required @@ record "body_text" sexp in
        Rss_to_mail.{ sender; to_; subject; body_html; body_text }
  in
  let feed_datas =
    match record "feed_data" sexp with
    | None -> empty.feed_datas
    | Some t -> List.fold_left parse_data empty.feed_datas (list (fun e -> e) t)
  and unsent_mails =
    match record "unsent" sexp with
    | None -> []
    | Some t -> list (List.map parse_unsent) t
  in
  { feed_datas; unsent_mails }
