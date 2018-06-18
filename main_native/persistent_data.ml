let feeds_file = "feeds.sexp"
let feed_datas_file = "feed_datas.sexp"

let record field =
	function
	| `List ts	->
		List.find_map (function
			| `List [ `Atom f; t ] when String.equal f field -> Some t
			| _ -> None) ts
	| `Atom _	-> failwith "Expecting record"

let list f =
	function
	| `List ts		-> f ts
	| `Atom _ as t	-> f [ t ]

let load_feeds () =
	let rec parse_options ~url ?cache ?label ?no_content =
		function
		| `List [ `Atom name; `Atom value ] :: tl ->
			begin match name with
				| "cache"		->
					let cache = float_of_string value in
					parse_options ~url ~cache ?label ?no_content tl
				| "label"		->
					parse_options ~url ?cache ~label:value ?no_content tl
				| "no_content"	->
					let no_content = bool_of_string value in
					parse_options ~url ?cache ?label ~no_content tl
				| opt			-> failwith ("Unknown option: " ^ opt)
			end
		| _ :: _	-> failwith (url ^ ": Malformed options")
		| []		-> Feed_options.make ?cache ?label ?no_content ()
	in
	let parse_feed =
		function
		| `List [ `Atom url; `List opts ] -> url, parse_options ~url opts
		| _ -> failwith "feeds: Syntax error"
	in
	match CCSexp.parse_file feeds_file with
	| exception Sys_error _ -> []
	| Error _		-> []
	| Ok t			->
		match record "feeds" t with
		| Some t	-> list (List.map parse_feed) t
		| None		-> failwith "Missing field `feeds`"

let load_feed_datas () : (int64 * SeenSet.t) StringMap.t * Rss_to_mail.mail list =
	let parse_ids set =
		function
		| `List [ `Atom id; `Atom date ] ->
			SeenSet.remove (Int64.of_string_exn date) id set
		| `Atom id	-> SeenSet.add id set
		| `List _	-> failwith ""
	in
	let parse_data m =
		function
		| `List [ `Atom url; `Atom date; `List ids ] ->
			let ids = List.fold_left parse_ids SeenSet.empty ids in
			StringMap.add url (Int64.of_string_exn date, ids) m
		| _ -> failwith ""
	in
	let parse_unsent =
		function
		| `List [ `Atom sender; `Atom subject; `Atom body ] ->
			Rss_to_mail.{ sender; subject; body }
		| _ -> failwith ""
	in
	let em = StringMap.empty in
	match CCSexp.parse_file feed_datas_file with
	| exception Sys_error _ -> em, []
	| Error _	-> em, []
	| Ok t		->
		record "feed_data" t |> Option.map_or em
			(list (List.fold_left parse_data em)),
		record "unsent" t |> Option.map_or []
			(list (List.map parse_unsent))

let save_feed_datas (datas, unsent) =
	let gen_id id removed lst =
		match removed with
		| Some date		->
			let date = Int64.to_string date in
			`List [ `Atom id; `Atom date ] :: lst
		| None			-> `Atom id :: lst
	in
	let gen_data uri (date, ids) lst =
		`List [ `Atom uri;
			`Atom (Int64.to_string date);
			`List (SeenSet.fold gen_id ids []) ] :: lst
	and gen_unsent t =
		let open Rss_to_mail in
		`List [ `Atom t.sender; `Atom t.subject; `Atom t.body ]
	in
	let datas = StringMap.fold gen_data datas []
	and unsent = List.map gen_unsent unsent in
	CCSexp.to_file feed_datas_file (`List [
		`List [ `Atom "feed_data"; `List datas ];
		`List [ `Atom "unsent"; `List unsent ]
	])
