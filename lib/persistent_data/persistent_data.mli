(** Serializer/deserializer for the `feed_datas.sexp` file. *)

module Feed_id : sig
  type t

  val of_url : string -> t
  val to_string : t -> string
end

module M : Rss_to_mail.STATE with type id = Feed_id.t

type t = {
  data : M.t;
  unsent_mails : Rss_to_mail.mail list;
}

val empty : t

val load : Sexplib0.Sexp.t list -> t
(** Raise [Failure _] on error. *)

val save : t -> Sexplib0.Sexp.t list
