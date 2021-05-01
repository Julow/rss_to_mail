(** Errors with context using exceptions. This module is meant to be opened. *)

(** Can be caught with {!handle_error}. *)
val raise_error : string -> 'a

val with_context : (unit -> string) -> (unit -> 'a) -> 'a

(** Raise [Failure] in case of error. *)
val handle_error : (unit -> 'a) -> 'a

val (let@) : ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a
