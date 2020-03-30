(** Serializer/deserializer for the `feed_datas.sexp` file. *)

module StringMap : module type of Map.Make (String)

type sexp =
  [ `Atom of string
  | `List of sexp list
  ]

type t = {
  feed_datas : (int64 * SeenSet.t) StringMap.t;
  unsent_mails : Rss_to_mail.mail list;
}

val empty : t

(** Raise [Failure _] on error. *)
val load : sexp -> t

val save : t -> sexp
