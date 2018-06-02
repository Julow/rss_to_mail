(** Persistent data
	Stores the sheet ID
	and all the feeds' data *)

type t

val load : unit -> t
val save : t -> unit

val create_empty : unit -> t

val get_sheet_id : t -> Js.js_string Js.t option
val set_sheet_id : t -> Js.js_string Js.t -> unit

val get_feed_data : t -> string -> (SeenSet.t * int64) option
val set_feed_data : t -> string -> SeenSet.t * int64 -> unit
