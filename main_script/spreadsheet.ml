type sheet_id = Js.js_string Js.t

let options_of_obj obj =
	let (=>) name f =
		let v = Js.Unsafe.get obj (Js.string name) in
		Js.Optdef.(to_option (map v f))
	in
	Feed_options.make ()
		?cache:("cache" => fun s -> `Every (Js.to_string (float_of_string s)))
		?label:("label" => Js.to_string)
		?no_content:("no_content" => Js.to_bool)

(** Read the spreadsheet, returns the list of (string * options) *)
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

(** Create the spreadsheet
	Returns its ID *)
let create () =
	let ss = SpreadsheetApp.t##create (Js.string "Rss to mail") 1 2 in
	let sheet = Js.array_get ss##getSheets 0 in
	let sheet = Js.Optdef.get sheet (fun () -> assert false) in
	sheet##appendRow (Js.array [| Js.string "Feed url"; Js.string "Options" |]);
	SpreadsheetApp.t##flush;
	ss##getId
