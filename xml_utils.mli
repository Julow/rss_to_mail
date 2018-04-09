exception Child_not_found of string
exception Attribute_not_found of string

type namespace
type node

(** Raises `Child_not_found` *)
val child : ?ns : namespace -> string -> node -> node
val children : ?ns : namespace -> string -> node -> node list

val tag : node -> string
val text : node -> string

(** Raises `Attribute_not_found` *)
val attribute : string -> node -> string

val namespace : string -> namespace
val parse : Js.js_string Js.t -> node

val node : node -> Script_API.XmlService.element Js.t

val create : ?ns : namespace -> string
	-> ?attr : (string * string) list
	-> node list
	-> node

val create_text : ?ns : namespace -> string
	-> ?attr : (string * string) list
	-> string
	-> node
