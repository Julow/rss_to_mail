type mail = {
  sender : string;
  to_ : string option;
  subject : string;
  body_html : string;
  body_text : string;
  timestamp : int64;
}

type 'a state_key =
  | Next_update : int64 state_key
  | Previous_entries : SeenSet.t state_key

module type FETCH = sig
  type error

  val fetch : Uri.t -> (string, error) result Lwt.t
end

module type STATE = sig
  type t
  type id

  val get : t -> id -> 'a state_key -> 'a option
  val set : t -> id -> 'a state_key -> 'a -> t
end

module Make (Fetch : FETCH) (State : STATE) : sig
  type feed = State.id * Feed_desc.t
  type update = { entries : int }

  type error =
    [ `Parsing_error of (int * int) * string
    | `Fetch_error of Fetch.error
    ]

  type log = State.id * [ `Updated of update | error | `Uptodate ]
  type nonrec mail = mail
  type nonrec 'a state_key = 'a state_key

  val check_all :
    now:int64 -> State.t -> feed list -> (State.t * mail list * log list) Lwt.t
  (** Update a list of feeds. Fetches are done asynchronously. *)
end
