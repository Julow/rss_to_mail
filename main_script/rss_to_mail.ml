let options_of_obj obj =
	let (=>) name f =
		let v = Js.Unsafe.get obj (Js.string name) in
		Js.Optdef.(to_option (map v f))
	in
	Feed_options.make ()
		?cache:("cache" => (Feed_options.cache_of_string % Js.to_string))
		?label:("label" => Js.to_string)
		?no_content:("no_content" => Js.to_bool)

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
				try
					try options_of_obj (Js._JSON##parse options_data##toString)
					with Js.Error e -> failwith (Js.string_of_error e)
				with Failure e ->
					Console.error ("Malformed options: "
						^ Js.to_string url##toString ^ ": " ^ e);
					Feed_options.make ()
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

(** `reqs` is an array of `(url, cache_timeout, process)`
	`cache_timeout` is in second
	`process` is a function that can process the result before it is cached *)
let cached_fetch_all reqs =
	let cache = CacheService.t##getScriptCache in
	let cache_put url res cache_time =
		let cache_time = int_of_float cache_time in
		try
			cache##put url (Json.output res) cache_time
		with Js.Error e ->
			let msg = Js.to_string e##.message and url = Js.to_string url in
			Console.error ("Cache put failed: " ^ msg ^ ": " ^ url)
	in
	let reqs = Array.of_list reqs in
	let urls = Array.map (fun (url, _, _) -> Js.string url) reqs in
	let cached =
		let cached = cache##getAll (Js.array urls) in
		fun index -> Js.Optdef.to_option (Js.Unsafe.get cached urls.(index))
	in
	let requests = new%js Js.array_empty in
	Array.iteri (fun index url ->
		if Option.is_none (cached index)
		then ignore (requests##push (object%js
				val url = url
				val muteHttpExceptions = Js._true
			end))) urls;
	let results = Js.to_array (UrlFetchApp.t##fetchAll requests) in
	let rec loop index req_index =
		if index >= Array.length reqs then []
		else match cached index with
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

let cut_entries since entries =
	Array.filter (fun e ->
		match e.Feed.date with
		| Some d	->
			let d = Int64.of_float (Js.date##parse (Js.string d)) in
			Int64.Infix.(>) d since
		| None		-> true) entries

let update_entry feed_url feed options entry =
	let open Feed in
	let id = match entry.id, entry.link, entry.title with
		| Some id, _, _				-> Some (feed_url ^ id)
		| None, Some link, _		-> Some (feed_url ^ Uri.to_string link)
		| None, None, Some title	-> Some (feed_url ^ title)
		| None, None, None			-> None
	and title = match entry.title, entry.link with
		| Some _ as title, _	-> title
		| None, Some link		-> Some (Uri.to_string link)
		| None, None			-> Some feed_url
	and content = Some (Mail_body.generate feed options entry) in
	{ entry with id; title; summary = None; content }

let parse_feed contents =
	Feed_parser.parse (Xmlm.make_input (`String (0, Js.to_string contents)))

class type params =
object
	method clear_cache_ : 'a. 'a Js.optdef Js.prop
end

let clear_cache data =
	let cache = CacheService.t##getScriptCache in
	let keys =
		Array.of_list data
		|> Array.map (fun (url, _) -> Js.string url)
		|> Js.array in
	cache##removeAll keys

let doGet (params : params Js.t) =
	let process_feed url options feed =
		feed.Feed.entries
		|> cut_entries (Int64.(sub (of_float Js.date##now) oldest_entry))
		|> Array.map (update_entry url feed options)
	in
	let process url options = function
		| `Ok contents	->
			begin try
				let feed = parse_feed contents in
				let feed = Feed.resolve_urls (Uri.of_string url) feed in
				let entries = process_feed url options feed in
				Console.info ("Fetched " ^ string_of_int (Array.length feed.entries)
					^ " entries (processed " ^ string_of_int (Array.length entries)
					^ ") from " ^ url);
				entries
			with Failure err ->
				Console.error ("Parsing error: " ^ err ^ ": " ^ url);
				[||]
			end
		| `Error code	->
			Console.error ("Fetch error: " ^ string_of_int code ^ ": " ^ url);
			[||]
	in
	Console.t##time (Js.string "all");
	let data = load_spreadsheet () in
	(if Js.Optdef.test params##.clear_cache_ then clear_cache data);
	let entries =
		data |> List.map (fun (url, options) ->
			let cache_time = options.Feed_options.cache *. 3600. in
			(url, cache_time, process url options))
		|> cached_fetch_all
		|> Array.concat
	in
	let output = Atom_format.generate {
			feed_title = Some "Feed aggregator";
			feed_link = None;
			feed_icon = None;
			entries
		} in
	Console.t##timeEnd (Js.string "all");
	let output = XmlService.t##getPrettyFormat##format_element output in
	ContentService.t##(createTextOutput output)
		##setMimeType ContentService.MimeType._ATOM

let () = Js.export "rss_to_mail"
	(object%js
		method doGet params = doGet params##.parameter
	end)
