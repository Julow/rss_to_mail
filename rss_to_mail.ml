open Script_API

module Feed_options =
struct

	type t = {
		cache	: float;
		name	: string option
	}

	let default = { cache = 1.; name = None }

	let of_obj obj =
		let cache = Js.Optdef.get obj##.cache (fun () -> default.cache)
		and name = Js.Optdef.case obj##.name (fun () -> default.name)
			(fun name -> Some (Js.to_string name)) in
		{ cache; name }

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
			let parse_options data =
				try Feed_options.of_obj (Js._JSON##parse data)
				with _ ->
					Logger.log ("Malformed options: " ^ Js.to_string data);
					Feed_options.default
			in
			match Js.to_array row with
			| [| url; options_data |] ->
				( Js.to_string url##toString, parse_options options_data##toString )
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
			cache##put url (Js._JSON##stringify res) cache_time
		with Js.Error e ->
			let e = Js.to_string e##.message in
			Logger.log ("Cannot cache " ^ Js.to_string url ^ ": " ^ e)
	in
	let reqs = Array.of_list reqs in
	let urls = Array.map (fun (url, _, _) -> Js.string url) reqs in
	let cached = cache##getAll (Js.array urls) in
	let cached = Array.map (fun url ->
			Js.Optdef.to_option (Jstable.find cached url)
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
				(Js._JSON##parse cached) :: loop (index + 1) req_index
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

let sort_entries = List.sort (fun a b -> Int64.compare b.Feed.date a.date)

let rec cut_entries since =
	function
	| entry :: _ as entries when entry.Feed.date <= since -> entries
	| _ :: tl	-> cut_entries since tl
	| []		-> []

let update_entry feed_url feed options entry =
	let open Feed in
	let content =
		let categories =
			let labels = List.map (fun c -> c.label) entry.categories in
			if labels = [] then ""
			else " (" ^ String.concat ", " labels ^ ")"
		in
		let title =
			match feed.feed_link with
			| Some link		->
				"<a href=\"" ^ link ^ "\">" ^ feed.feed_title ^ "</a>"
			| None			-> feed.feed_title
		and author =
			match entry.author with
			| Some author	-> " by " ^ author
			| None			-> ""
		in
		"Via " ^ title ^ categories ^ "<br/>"
		^ "on " ^ entry_date_string entry ^ author ^ "<br/>"
		^ "<a href=\"" ^ entry.link ^ "\">" ^ entry.title ^ "</a>\n"
		^ "\n" ^ entry.content
	and id = feed_url ^ entry.id in
	{ entry with id; content }

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
	let process url options = function
		| `Ok contents	->
			begin try
				let feed = parse_feed contents in
				sort_entries feed.entries
				|> cut_entries (Int64.(sub (of_float Js.date##now) oldest_entry))
				|> List.map (update_entry url feed options)
			with Failure err ->
				Logger.log (Printf.sprintf "Parsing failed for %s: %s" url err);
				[]
			end
		| `Error code	->
			Logger.log (Printf.sprintf "Fetch error for %s: %d" url code);
			[]
	in
	let entries =
		load_spreadsheet ()
		|> List.map (fun (url, options) ->
			let cache_time = options.Feed_options.cache *. cache_base_time in
			( url, cache_time, process url options ))
		|> cached_fetch_all
		|> List.concat
		|> sort_entries
	in
	let output = Xml_utils.node @@ Atom_format.generate {
			feed_title = "Feed aggregator";
			feed_link = None;
			entries
		} in
	let output = XmlService.t##getPrettyFormat##format_element output in
	ContentService.t##(createTextOutput output)
		##setMimeType ContentService.MimeType._ATOM

let () = Js.export "rss_to_mail"
	(object%js
		method doGet = doGet ()
	end)
