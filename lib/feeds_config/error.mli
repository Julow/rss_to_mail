(** Errors with context using exceptions. This module is meant to be opened. *)

val raise_error : string -> 'a
(** Can be caught with {!handle_error}. *)

val with_context : (unit -> string) -> (unit -> 'a) -> 'a
val with_context_field : string -> (unit -> 'a) -> 'a

val handle_error : (unit -> 'a) -> 'a
(** Raise [Failure] in case of error. *)

val ( let@ ) : ((unit -> 'a) -> 'a) -> (unit -> 'a) -> 'a
