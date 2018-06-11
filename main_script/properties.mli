(** Persistent data
	Stores the sheet ID
	and all the feeds' data *)

type t

val load : unit -> t

val get_sheet_id : t -> Js.js_string Js.t option
val get_feed_data : t -> string -> (SeenSet.t * int64) option
val get_unsent_mails : t -> (string * string * string) list

val set_sheet_id : Js.js_string Js.t -> unit
val set_feed_data : string -> SeenSet.t * int64 -> unit
val set_unsent_mails : (string * string * string) list -> unit
