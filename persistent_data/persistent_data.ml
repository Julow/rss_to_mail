(** Parser for the `feeds.sexp` config file
	and serializer/deserializer for the `feed_datas.sexp` persistent file
	Used by main_native *)

let record field =
	function
	| `List ts	->
		List.find_map (function
			| `List [ `Atom f; t ] when String.equal f field -> Some t
			| _ -> None) ts
	| `Atom _	-> failwith "Expecting record"

let atom =
	function
	| `List _	-> failwith "Expecting string"
	| `Atom s	-> s

let list f =
	function
	| `List ts		-> f ts
	| `Atom _ as t	-> f [ t ]

let parse_scraper =
	let open Scrap in
	let rec scraper ~target =
		function
		| `List (`Atom "R" :: rs)	-> R (List.map (rule ~target) rs)
		| `List (`Atom "T" :: ts)	-> T (List.map target ts)
		| _							-> failwith "Malformated scraper"
	and rule ~target =
		function
		| `List [ `Atom sel; t ]	-> sel, scraper ~target t
		| _							-> failwith "Malformated scraper rule"
	in
	let open Scraper in
	let target =
		function
		| `Atom "feed_title"			-> Feed_title
		| `Atom "feed_icon"				-> Feed_icon
		| `List [ `Atom "entry"; t ]	->
			let target =
				function
				| `Atom "title"		-> Title
				| `Atom "link"		-> Link
				| _					-> failwith "Invalid target"
			in
			Entry (scraper ~target t)
		| _								-> failwith "Invalid target"
	in
	fun t -> R [ rule ~target t ]

type config = {
	smtp	: string * [ `Plain of string * string ] option;
	address	: string;
	feeds	: (string * Feed_options.t) list
}

let load_feeds file =
	let rec parse_options (options : Feed_options.t) =
		function
		| `List [ `Atom name; value ] :: tl ->
			let options = match name with
				| "cache"		->
					let cache = Feed_options.cache_of_string (atom value) in
					{ options with cache }
				| "label"		->
					let label = Some (atom value) in
					{ options with label }
				| "no_content"	->
					let no_content = bool_of_string (atom value) in
					{ options with no_content }
				| "scraper"		->
					let scraper = Some (parse_scraper value) in
					{ options with scraper }
				| opt			-> failwith ("Unknown option: " ^ opt)
			in
			parse_options options tl
		| _ :: _	-> failwith "Malformated options"
		| []		-> options
	in
	let parse_feed =
		function
		| `Atom url					-> url, Feed_options.make ()
		| `List (`Atom url :: opts)	->
			(try url, parse_options (Feed_options.make ()) opts
			with Failure msg -> failwith (url ^ ": " ^ msg))
		| _ -> failwith "feeds: Syntax error"
	in
	match CCSexp.parse_file file with
	| exception Sys_error msg	-> failwith msg
	| Error msg					-> failwith msg
	| Ok t						->
		let feeds = match record "feeds" t with
			| Some t	-> list (List.map parse_feed) t
			| None		-> failwith "Missing field `feeds`"
		and smtp = match record "smtp" t with
			| Some (`List [ `Atom serv ]) -> serv, None
			| Some (`List [ `Atom serv; `List [ `Atom user; `Atom pass ] ]) ->
				serv, Some (`Plain (user, pass))
			| Some _	-> failwith "Malformated field `smtp`"
			| None		-> failwith "Missing field `smtp`"
		and address = match record "address" t with
			| Some (`Atom a)	-> a
			| Some _			-> failwith "Malformated field `address`"
			| None				-> failwith "Missing field `address`"
		in
		{ smtp; address; feeds }

let load_feed_datas feed_datas_file : (int64 * SeenSet.t) StringMap.t * Rss_to_mail.mail list =
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

let save_feed_datas feed_datas_file (datas, unsent) =
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
