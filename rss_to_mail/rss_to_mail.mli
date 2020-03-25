type mail = Mail_body.t = {
  sender : string;
  subject : string;
  body_html : string;
  body_text : string;
}

type feed_data = int64 * SeenSet.t
(** [last_update * seen_ids] *)

module Make (Fetch : sig
  type error

  val fetch : Uri.t -> (string, error) result Lwt.t
end) (Feed_datas : sig
  type t

  val get : t -> string -> feed_data option

  val set : t -> string -> feed_data -> t
end) : sig
  type update = { entries : int }

  type error =
    [ `Parsing_error of (int * int) * string
    | `Fetch_error of Fetch.error
    ]

  type log = string * [ `Updated of update | error | `Uptodate ]

  type nonrec mail = mail

  type nonrec feed_data = feed_data

  val check_all :
    now:int64 ->
    Feed_datas.t ->
    Feed_desc.t list ->
    (Feed_datas.t * mail list * log list) Lwt.t
  (** Update a list of feeds. Fetches are done asynchronously. *)
end
