(** Stores IDs that have been seen
	Allow to remove an ID in the future *)

type t

(** An empty set *)
val empty : t

(** Check if an ID is seen *)
val is_seen : string -> t -> bool

(** Add an ID to the set *)
val add : string -> t -> t

(** Remove an ID later
	It will be removed when [filter_removed] is called
	with a [since] value greater or equal than [date]
	Removing again the same ID will override the previous request *)
val remove : Int64.t -> string -> t -> t

(** Remove an ID, immediately *)
val remove_now : string -> t -> t

(** Remove IDs that are to be removed at a date lower than [since] *)
val filter_removed : Int64.t -> t -> t
