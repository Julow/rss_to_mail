(** Parse a feed from a Xmlm.input stream Supports both RSS and Atom Silently
    ignore errors as much as possible Raise [Failure] on fatal error *)

exception Error of (int * int) * string
(** Raised on parsing error (line * column) * message *)

let ns = function
  | "media" -> Some "http://search.yahoo.com/mrss/"
  | "content" -> Some "http://purl.org/rss/1.0/modules/content/"
  | "dc" -> Some "http://purl.org/dc/elements/1.1/"
  | ns -> Some ns

let parse ~resolve_uri source =
  let inp = Xmlm.make_input ~ns source in
  try
    let node = Xml.parse_from_begining inp in
    match Xml.name node with
    | "rss" -> Rss.parse ~resolve_uri node
    | "feed" -> Atom.parse ~resolve_uri node
    | _ -> failwith "Unsupported format"
  with
  | Failure msg -> raise (Error (Xmlm.pos inp, msg))
  | Xmlm.Error (pos, err) -> raise (Error (pos, Xmlm.error_message err))
