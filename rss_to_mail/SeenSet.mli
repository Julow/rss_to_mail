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
val remove : int64 -> string -> t -> t

(** Remove an ID, immediately *)
val remove_now : string -> t -> t

(** Remove IDs that are to be removed at a date lower than [since] *)
val filter_removed : int64 -> t -> t

(** Adds (or cancels remove) of the IDs in [new_ids]
	Remove IDs that are not in [new_ids] at [date] *)
val new_ids : int64 -> string list -> t -> t

(** Build a SeenSet from a list of (ID * remove date) *)
val of_list : (string * int64 option) list -> t

(** To (ID, remove date) list, can be constructed back with [of_list] *)
val to_list : t -> (string * int64 option) list

(** Like [of_list] followed by [filter_removed] *)
val of_list_filter : int64 -> (string * int64 option) list -> t
