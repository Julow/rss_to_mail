(** Serializer/deserializer for the `feed_datas.sexp` file. *)

module Feed_id : sig
  type t

  val of_url : string -> t
  val to_string : t -> string
end

module Feed_map : Map.S with type key = Feed_id.t

type feed_data = int64 * SeenSet.t

module M : sig
  type t = feed_data Feed_map.t
  type id = Feed_id.t

  val get : t -> id -> feed_data option
  val set : t -> id -> feed_data -> t
end

type t = {
  feed_datas : M.t;
  unsent_mails : Rss_to_mail.mail list;
}

val empty : t

val load : Sexplib0.Sexp.t list -> t
(** Raise [Failure _] on error. *)

val save : t -> Sexplib0.Sexp.t list
