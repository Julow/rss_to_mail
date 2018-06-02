(** Abstraction over UrlFetchApp *)

type 'a req

(** Build a [req]
	The callback funtion will be called by [perform] *)
val url : string -> ((string, int) result -> 'a) -> 'a req

(** Perform fetches *)
val perform : 'a req list -> 'a list
