type node = Text of string | Node of Xmlm.tag * node list

let name_equal ns name = function
	| Text _						-> false
	| Node (((ns', name'), _), _)	->
		String.(equal name name' && equal ns ns')

let name =
	function
	| Text txt				-> failwith "Xml.name: text"
	| Node (((_, n), _), _)	-> n

let child ?(ns="") name = function
	| Text _			-> None
	| Node (_, childs)	-> List.find_opt (name_equal ns name) childs

let child_exn ?(ns="") name node =
	match child ~ns name node with
	| Some c		-> c
	| None			->
		let name = if String.is_empty ns then name else ns ^ ":" ^ name in
		failwith ("Element not found: " ^ name)

let children ?(ns="") name = function
	| Text _			-> []
	| Node (_, childs)	-> List.filter (name_equal ns name) childs

let attr ?(ns="") key = function
	| Text _				-> None
	| Node ((_, attrs), _)	->
		match List.find (fun ((ns', key'), _) ->
			String.(equal key key' && equal ns ns')) attrs with
		| exception Not_found	-> None
		| _, v					-> Some v

let rec text b = function
	| Text txt :: tl			-> Buffer.add_string b txt; text b tl
	| Node (_, childs) :: tl	-> text b childs; text b tl
	| []						-> ()

let text =
	function
	| Text txt			-> txt
	| Node (_, childs)	->
		let b = Buffer.create 32 in
		text b childs;
		Buffer.contents b

let rec parse nodes inp =
	match Xmlm.input inp with
	| `El_end			-> List.rev nodes
	| `El_start tag		-> parse (Node (tag, parse [] inp) :: nodes) inp
	| `Data txt			-> parse (Text txt :: nodes) inp
	| `Dtd _			-> parse nodes inp

let parse inp = parse [] inp

let rec parse_from_begining inp =
	match Xmlm.input inp with
	| `Dtd _			-> parse_from_begining inp
	| `El_start tag		-> Node (tag, parse inp)
	| `El_end | `Data _	-> failwith "Invalid format"
