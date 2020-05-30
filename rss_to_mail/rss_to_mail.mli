type mail = {
  sender : string;
  to_ : string option;
  subject : string;
  body_html : string;
  body_text : string;
}

type feed_data = int64 * SeenSet.t
(** [next_update * seen_ids] *)

module Make (Fetch : sig
  type error

  val fetch : Uri.t -> (string, error) result Lwt.t
end) (Feed_datas : sig
  type t

  type id

  val get : t -> id -> feed_data option

  val set : t -> id -> feed_data -> t
end) : sig
  type update = { entries : int }

  type error =
    [ `Parsing_error of (int * int) * string
    | `Fetch_error of Fetch.error
    ]

  type log = Feed_datas.id * [ `Updated of update | error | `Uptodate ]

  type nonrec mail = mail

  type nonrec feed_data = feed_data

  val check_all :
    now:int64 ->
    Feed_datas.t ->
    (Feed_datas.id * Feed_desc.t) list ->
    (Feed_datas.t * mail list * log list) Lwt.t
  (** Update a list of feeds. Fetches are done asynchronously. *)
end
