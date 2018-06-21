type target =
	| Tag of string
	| Class of string
	| Id of string
	| Any

type sel = target list

type child = Direct of sel | Descend of sel

(** CSS-like selector *)
type t = child list

(** Parse a string into a list of selector
	Raises [Failure] on syntax error *)
val parse : string -> t list
