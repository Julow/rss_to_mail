(** Persistent data
	Stores the sheet ID
	and all the feeds' data *)

type t

val load : unit -> t

val get_sheet_id : t -> Js.js_string Js.t option
val get_feed_data : t -> string -> (int64 * SeenSet.t) option
val get_unsent_mails : t -> Rss_to_mail.mail list

val set_sheet_id : Js.js_string Js.t -> unit
val set_feed_data : string -> int64 * SeenSet.t -> unit
val set_unsent_mails : Rss_to_mail.mail list -> unit
