(** Parse a feed from a Xmlm.input stream
	Supports both RSS and Atom
	Silently ignore errors as much as possible
	Raise [Failure] on fatal error *)

(** Raised on parsing error
	(line * column) * message *)
exception Error of (int * int) * string

let parse inp =
	try
		let node = Xml.parse_from_begining inp in
		match Xml.name node with
		| "rss"		-> Rss.parse node
		| "feed"	-> Atom.parse node
		| _			-> failwith "Unsupported format"
	with Failure msg ->
		raise (Error (Xmlm.pos inp, msg))
	| Xmlm.Error (pos, err) ->
		raise (Error (pos, Xmlm.error_message err))
