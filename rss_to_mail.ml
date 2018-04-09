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
	let requests = new%js Js.array_empty
	and callbacks = new%js Js.array_empty in
	let fetch url cache_timeout callback =
		let callback res =
			let res =
				let code = res##getResponseCode in
				if code <> 200
				then `Fetch_error code
				else `Success res##getContentText
			in
			begin
				try cache##put url (Js._JSON##stringify res) cache_timeout
				with Js.Error e ->
					let e = Js.to_string e##.message
					and url = Js.to_string url in
					Logger.log ("Cannot cache " ^ url ^ ": " ^ e)
					| _ -> Logger.log "WTF, cache not Js.Error"
			end;
			callback res
		in
		ignore (requests##push (object%js
				val url = url
				val muteHttpExceptions = Js._true
			end));
		ignore (callbacks##push callback)
	in
	List.iter (fun (url, cache_timeout, callback) ->
		let url = Js.string url
		and cache_timeout = int_of_float cache_timeout in
		Js.Opt.case (cache##get url)
			(fun () -> fetch url cache_timeout callback)
			(fun data -> callback (Js._JSON##parse data))
	) reqs;
	if requests##.length > 0 then
		let results = UrlFetchApp.t##fetchAll requests in
		Array.iter2 (fun f res -> f res)
			(Js.to_array callbacks) (Js.to_array results)

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
			| Some link	-> "<a href=\"" ^ link ^ "\">" ^ feed.feed_title ^ "</a>"
			| None		-> feed.feed_title
		in
		"Via " ^ title ^ categories ^ "<br/>"
		^ "on " ^ entry_date_string entry ^ " by " ^ entry.author ^ "<br/>"
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
	let entries = ref [] in
	let process url options = function
		| `Fetch_error code	->
			Logger.log (Printf.sprintf "Fetch error for %s: %d" url code)
		| `Success contents	->
			try
				let feed = parse_feed contents in
				sort_entries feed.entries
				|> cut_entries (Int64.(sub (of_float Js.date##now) oldest_entry))
				|> List.map (update_entry url feed options)
				|> fun e -> entries := e @ !entries
			with Failure err ->
				Logger.log (Printf.sprintf "Parsing failed for %s: %s" url err)
	in
	load_spreadsheet ()
	|> List.map (fun (url, options) ->
		let cache = options.Feed_options.cache *. cache_base_time in
		( url, cache, process url options ))
	|> cached_fetch_all;
	let output = Xml_utils.node @@ Atom_format.generate {
			feed_title = "Feed aggregator";
			feed_link = None;
			entries = sort_entries !entries
		} in
	let output = XmlService.t##getPrettyFormat##format_element output in
	ContentService.t##(createTextOutput output)
		##setMimeType ContentService.MimeType._ATOM

let () = Js.export "rss_to_mail"
	(object%js
		method doGet = doGet ()
	end)
