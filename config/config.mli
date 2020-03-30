(** Parse Rss_to_mail's configuration *)

type t = {
  server : string * int;
  server_auth : [ `Plain of string * string ];
  from_address : string;
  to_address : string;
  feeds : Feed_desc.t list;
}

(** Raise [Failure _] on error. Errors don't have a location. *)
val parse : Sexplib0.Sexp.t -> t

(** Raise [Failure _] on error. *)
val parse_scraper : Sexplib0.Sexp.t list -> Scraper.t
