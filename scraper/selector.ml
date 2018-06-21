type target =
	| Tag of string
	| Class of string
	| Id of string
	| Any

type sel = target list

type child = Direct of sel | Descend of sel

type t = child list

let error i msg = failwith (string_of_int i ^ ": " ^ msg)

let is_id =
	function
	| 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' -> true
	| _ -> false

let rec skip f s i =
	if i < String.length s && f s.[i]
	then skip f s (i + 1)
	else i

let parse_id s i =
	let ends = skip is_id s i in
	if i = ends
	then error i "Identifier expected"
	else String.sub s i (ends - i), ends

let _Descend s = Descend s
let _Direct s = Direct s

(** Quick and dirty parser *)
let rec parse ts childs create_child targets s i =
	let collapse_childs () =
		if List.is_empty targets
		then childs
		else create_child (List.rev targets) :: childs
	in
	let collapse_t () =
		match collapse_childs () with
		| []		-> ts
		| childs	-> List.rev childs :: ts
	in
	let with_target t i = parse ts childs create_child (t :: targets) s i in
	if i >= String.length s
	then List.rev (collapse_t ())
	else match s.[i] with
		| ' ' | '\n' | '\t' when List.is_empty targets ->
			parse ts childs create_child [] s (i + 1)
		| ' ' | '\n' | '\t'	->
			let childs = create_child (List.rev targets) :: childs in
			parse ts childs _Descend [] s (i + 1)
		| ','				-> parse (collapse_t ()) [] _Descend [] s (i + 1)
		| '>'				->
			let childs = collapse_childs () in
			parse ts childs _Direct [] s (i + 1)
		| '#'				->
			let id, i = parse_id s (i + 1) in
			with_target (Id id) i
		| '.'				->
			let id, i = parse_id s (i + 1) in
			with_target (Class id) i
		| '*'				-> with_target Any (i + 1)
		| c when is_id c	->
			let id, i = parse_id s i in
			with_target (Tag id) i
		| _					-> error i "Syntax error"

let parse s = parse [] [] _Descend [] s 0
