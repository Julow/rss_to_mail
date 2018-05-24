exception Child_not_found of string
exception Attribute_not_found of string

type namespace
type node

(** [child ~ns "tag" node]
	Returns the child node of [node] with tag ["tag"] and namespace [ns]
	Raises `Child_not_found` if it does not exist *)
val child : ?ns : namespace -> string -> node -> node

(** [children ~ns "tag" node]
	Returns all the child nodes of [node] with tag ["tag"] and namespace [ns] *)
val children : ?ns : namespace -> string -> node -> node list

(** [child_opt f ~ns "tag" node]
	Likes [child] but calls [f] on the child node if it exists
		and use the [option] type instead of raising *)
val child_opt : (node -> 'a) -> ?ns : namespace -> string -> node -> 'a option

val tag : node -> string
val text : node -> string
val raw_text : node -> Js.js_string Js.t

(** Raises `Attribute_not_found` *)
val attribute : string -> node -> string

val attribute_opt : (string -> 'a) -> string -> node -> 'a option

val namespace : string -> namespace
val parse : Js.js_string Js.t -> node

val node : node -> XmlService.element Js.t

val create : ?ns : namespace -> string
	-> ?attr : (string * string) list
	-> node list
	-> node

val create_text : ?ns : namespace -> string
	-> ?attr : (string * string) list
	-> string
	-> node

val create_raw_text : ?ns : namespace -> string
	-> ?attr : (string * string) list
	-> Js.js_string Js.t
	-> node
