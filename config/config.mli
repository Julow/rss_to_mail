(** Parse Rss_to_mail's configuration *)

type sexp =
  [ `Atom of string
  | `List of sexp list
  ]

type t = {
  server : string * int;
  server_auth : [ `Plain of string * string ];
  from_address : string;
  to_address : string;
  feeds : Feed_desc.t list;
}

(** Raise [Failure _] on error. Errors don't have a location. *)
val parse : sexp -> t

(** Raise [Failure _] on error. *)
val parse_scraper : sexp list -> Scraper.t
