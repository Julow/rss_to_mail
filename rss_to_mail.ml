open Script_API

module Feed_options =
struct

	type t = {
		cache		: float;
		name		: string option;
		no_content	: bool
	}

	let of_obj obj =
		let prop name default map =
			let v = Js.Unsafe.get obj (Js.string name) in
			Js.Optdef.case v (fun () -> default) map
		in
		let some_string s = Some (Js.to_string s) in
		{	cache		= prop "cache"		1.		Js.to_float;
			name		= prop "name"		None	some_string;
			no_content	= prop "no_content"	false	Js.to_bool }

	let default () = of_obj (object%js end)

end

module Feed_spreadsheet =
struct

	type sheet_id = Js.js_string Js.t

	let read sheet_id =
		let ss = SpreadsheetApp.t##openById sheet_id in
		let sheet = Js.array_get ss##getSheets 0 in
		let sheet = Js.Optdef.get sheet (fun () -> failwith "Empty spreadsheet") in
		let last_row = sheet##getLastRow in
		let process_row row =
			let parse_options url options_data =
				try Feed_options.of_obj (Js._JSON##parse options_data##toString)
				with _ ->
					Console.error "Malformed options: %s" (Js.to_string url##toString);
					Feed_options.default ()
			in
			match Js.to_array row with
			| [| url; options_data |] ->
				(Js.to_string url##toString, parse_options url options_data)
			| _ -> assert false
		in
		if last_row <= 1
		then []
		else sheet##(getRange 2 1 (last_row - 1) 2)##getValues
			|> Js.to_array |> Array.map process_row |> Array.to_list

	let create () =
		let ss = SpreadsheetApp.t##create (Js.string "Rss to mail") 1 2 in
		let sheet = Js.array_get ss##getSheets 0 in
		let sheet = Js.Optdef.get sheet (fun () -> assert false) in
		sheet##appendRow (Js.array [| Js.string "Feed url"; Js.string "Options" |]);
		SpreadsheetApp.t##flush;
		ss##getId

end

let cached_fetch_all reqs =
	let cache = CacheService.t##getScriptCache in
	let cache_put url res cache_time =
		let cache_time = int_of_float cache_time in
		try
			cache##put url (Json.output res) cache_time
		with Js.Error e ->
			let msg = Js.to_string e##.message and url = Js.to_string url in
			Console.error "Cache put failed: %s: %s" msg url
	in
	let reqs = Array.of_list reqs in
	let urls = Array.map (fun (url, _, _) -> Js.string url) reqs in
	let cached = cache##getAll (Js.array urls) in
	let cached = Array.map (fun url ->
			Js.Optdef.to_option
			(Js.Unsafe.get cached url : Js.js_string Js.t Js.optdef)
		) urls in
	let requests = new%js Js.array_empty in
	Array.iteri (fun index url ->
		if cached.(index) = None
		then ignore (requests##push (object%js
				val url = url
				val muteHttpExceptions = Js._true
			end))) urls;
	let results = Js.to_array (UrlFetchApp.t##fetchAll requests) in
	let rec loop index req_index =
		if index >= Array.length cached then []
		else match cached.(index) with
			| Some cached	->
				(Json.unsafe_input cached) :: loop (index + 1) req_index
			| None			->
				let _, cache_time, process = reqs.(index) in
				let res =
					let res = results.(req_index) in
					if res##getResponseCode <> 200
					then process (`Error res##getResponseCode)
					else process (`Ok res##getContentText)
				in
				cache_put urls.(index) res cache_time;
				res :: loop (index + 1) (req_index + 1)
	in
	loop 0 0

let cache_base_time = 60. *. 30.
let oldest_entry = Int64.of_int (7 * 24 * 3600000)

let load_spreadsheet () =
	let properties = PropertiesService.t##getUserProperties in
	let sheet_id_prop = Js.string "SHEET_ID" in
	Js.Opt.case (properties##getProperty sheet_id_prop)
		(fun () ->
			let sheet_id = Feed_spreadsheet.create () in
			properties##setProperty sheet_id_prop sheet_id;
			[])
		Feed_spreadsheet.read

let sort_entries entries =
	let entries = Array.copy entries in
	Array.sort (fun a b -> Int64.compare b.Feed.date a.date) entries;
	entries

let rec cut_entries i since entries =
	if i < Array.length entries && entries.(i).Feed.date > since
	then cut_entries (i + 1) since entries
	else Array.sub entries 0 i

let update_entry feed_url feed options entry =
	let open Feed in
	let summary =
		let opt_link title = function
			| Some link	-> "<a href=\"" ^ link ^ "\">" ^ title ^ "</a>"
			| None		-> title
		in
		let categories =
			let labels = List.map (function
				| { label = Some l; _ }	-> l
				| { term = Some t; _ }	-> t
				| _ -> "") entry.categories in
			if labels = [] then "" else " (" ^ String.concat ", " labels ^ ")"
		and authors =
			let author a = opt_link a.author_name a.author_link in
			if entry.authors = [] then ""
			else " by " ^ String.concat ", " (List.map author entry.authors)
		and summary =
			match entry.summary with
			| Some sum	-> "<br/>" ^ sum
			| None		-> ""
		in
		"Via " ^ opt_link feed.feed_title feed.feed_link ^ categories ^ "<br/>"
		^ "on " ^ entry_date_string entry ^ authors ^ "<br/>"
		^ opt_link entry.title entry.link
		^ summary
	in
	let content =
		match entry.content, options.Feed_options.no_content with
		| Some c, false	-> Some (summary ^ "<br/><br/>" ^ c)
		| _				-> Some summary
	in
	let id = match entry.id with
		| Some id	-> Some (feed_url ^ id)
		| None		-> Some (feed_url ^ entry.title)
	in
	{ entry with id; summary = None; content }

let parse_feed contents =
	let open Xml_utils in
	try
		let root = parse contents in
		match tag root with
		| "rss"		-> Rss_format.parse root
		| "feed"	-> Atom_format.parse root
		| _			-> failwith "Unexpected format"
	with
	| Js.Error err				-> failwith (Js.to_string err##.message)
	| Child_not_found tag		-> failwith ("Missing tag " ^ tag)
	| Attribute_not_found attr	-> failwith ("Missing attribute " ^ attr)

let doGet () =
	let process_feed url options feed =
		sort_entries feed.Feed.entries
		|> cut_entries 0 (Int64.(sub (of_float Js.date##now) oldest_entry))
		|> Array.map (update_entry url feed options)
	in
	let process url options = function
		| `Ok contents	->
			begin try
				let feed = parse_feed contents in
				let entries = process_feed url options feed in
				Console.info "Fetched %d entries (processed %d) from %s"
					(Array.length feed.entries) (Array.length entries) url;
				entries
			with Failure err ->
				Console.error "Parsing error: %s: %s" err url;
				[||]
			end
		| `Error code	->
			Console.error "Fetch error: %d: %s" code url;
			[||]
	in
	Console.t##time (Js.string "all");
	let entries =
		load_spreadsheet ()
		|> List.map (fun (url, options) ->
			let cache_time = options.Feed_options.cache *. cache_base_time in
			( url, cache_time, process url options ))
		|> cached_fetch_all
		|> Array.concat
		|> sort_entries
	in
	let output = Xml_utils.node @@ Atom_format.generate {
			feed_title = "Feed aggregator";
			feed_link = None;
			entries
		} in
	Console.t##timeEnd (Js.string "all");
	let output = XmlService.t##getPrettyFormat##format_element output in
	ContentService.t##(createTextOutput output)
		##setMimeType ContentService.MimeType._ATOM

let () = Js.export "rss_to_mail"
	(object%js
		method doGet = doGet ()
	end)
