(** Eagerly parse a full XML document *)

type node

val name : node -> string

val child : ?ns:string -> string -> node -> node option
val child_exn : ?ns:string -> string -> node -> node
val children : ?ns:string -> string -> node -> node list

val attr : ?ns:string -> string -> node -> string option

val text : node -> string

val parse : Xmlm.input -> node list
val parse_from_begining : Xmlm.input -> node
