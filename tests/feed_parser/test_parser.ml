open Feed
open Printf

let p label f = function
	| None		-> ()
	| Some s	-> printf "%-20s %s\n" label (f s)

let mapopt f = function
	| []				-> None
	| (_ :: _) as lst	-> Some (List.map f lst)

let print_entry e =
	let author a =
		match a.author_link with
		| Some l	-> a.author_name ^ " (" ^ Uri.to_string l ^ ")"
		| None		-> a.author_name
	and category c =
		[ c.label; Option.map (sprintf "(%s)") c.term ]
		|> List.filter_map id |> String.concat " "
	in
	p "	Title" id e.title;
	p "	Id" id e.id;
	p "	Link" Uri.to_string e.link;
	p "	Author" (String.concat ", ") (mapopt author e.authors);
	p "	Category" (String.concat ", ") (mapopt category e.categories);
	p "	Summary" (string_of_int % String.length) e.summary;
	p "	Content" (string_of_int % String.length) e.content;
	p "	Thumbnail" Uri.to_string e.thumbnail;
	p "	Date" id e.date;
	List.iteri (fun i att ->
		p "	Attachment" string_of_int (Some i);
		p "		Url" Uri.to_string (Some att.attach_url);
		p "		Size" Int64.to_string att.attach_size;
		p "		Type" id att.attach_type
	) e.attachments

let () =
	let input = Xmlm.make_input (`Channel (open_in Sys.argv.(1))) in
	let feed = Feed_parser.parse input in
	p "Feed title" id feed.feed_title;
	p "Feed link" Uri.to_string feed.feed_link;
	p "Feed icon" Uri.to_string feed.feed_icon;
	Array.iteri (fun i e ->
		p "Entry" string_of_int (Some i);
		print_entry e
	) feed.entries
