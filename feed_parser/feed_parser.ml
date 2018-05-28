(** Parse a feed from a Xmlm.input stream
	Supports both RSS and Atom
	Silently ignore errors as much as possible
	Raise [Failure] on fatal error *)

let fatal_error line col msg =
	failwith (Printf.sprintf "Error: %d:%d: %s" line col msg)

let parse inp =
	try
		let node = Xml.parse_from_begining inp in
		match Xml.name node with
		| "rss"		-> Rss.parse node
		| "feed"	-> Atom.parse node
		| _			-> failwith "Unsupported format"
	with Failure msg ->
		let line, col = Xmlm.pos inp in
		fatal_error line col msg
	| Xmlm.Error ((line, col), err) ->
		fatal_error line col (Xmlm.error_message err)
