open Xml_utils
open Feed

let ns = namespace "http://www.w3.org/2005/Atom"
let media_ns = namespace "http://search.yahoo.com/mrss/"

let parse feed_elem =
	let parse_entry entry =
		let date =
			Int64.of_float @@
			Js.date##parse (node (child ~ns "updated" entry))##getText
		and authors =
			let parse_author author =
				{	author_name = text (child ~ns "name" author);
					author_link = child_opt text ~ns "uri" author }
			in
			List.map parse_author @@ children ~ns "author" entry
		and categories =
			let parse_category cat =
				let attr_opt k = try Some (attribute k cat) with _ -> None in
				{ term = attr_opt "term"; label = attr_opt "label" }
			in
			List.map parse_category @@ children ~ns "category" entry
		and thumbnail =
			child_opt (attribute "url") ~ns:media_ns "thumbnail" entry
		in
		{	id = child_opt text ~ns "id" entry;
			title = text (child ~ns "title" entry);
			summary = child_opt text ~ns "summary" entry;
			content = child_opt (fun n -> (node n)##getText) ~ns "content" entry;
			link = child_opt (attribute "href") ~ns "link" entry;
			thumbnail; authors; date; categories }
	in
	let feed_title = text (child ~ns "title" feed_elem)
	and feed_icon = child_opt text ~ns "icon" feed_elem
	and feed_link =
		try
			let alternate link =
				try attribute "rel" link = "alternate"
				with _ -> false in
			List.find alternate (children ~ns "link" feed_elem)
			|> fun link -> Some (attribute "href" link)
		with _ -> None
	and entries =
		Array.of_list (children ~ns "entry" feed_elem)
		|> Array.map parse_entry
	in
	{ feed_title; feed_link; feed_icon; entries }

let generate feed =
	let gen_entry entry =
		let gen_category cat =
			let opt k = function Some v -> [ k, v ] | None -> [] in
			let attr = opt "term" cat.term @ opt "label" cat.label in
			create ~ns "category" ~attr []
		and gen_author { author_name; author_link } =
			let uri = match author_link with
				| Some link	-> [ create_text ~ns "uri" link ]
				| None		-> []
			and name = create_text ~ns "name" author_name in
			create ~ns "author" (name :: uri)
		in
		(** option map to_list *)
		let m t f = match t with Some v -> [ f v ] | None -> [] in
		create ~ns "entry" ([]
			@ List.map gen_author entry.authors
			@ m entry.summary
				(create_text ~ns "summary" ~attr:[ "type", "html" ])
			@ m entry.content
				(create_raw_text ~ns "content" ~attr:[ "type", "html" ])
			@ m entry.id (create_text ~ns "id")
			@ m entry.link (fun link ->
				create ~ns "link" ~attr:[ "href", link ] [])
			@ m entry.thumbnail (fun url ->
				create ~ns:media_ns "thumbnail" ~attr:[ "url", url ] [])
			@ create_text ~ns "title" entry.title
			:: create_text ~ns "updated" (entry_date_string entry)
			:: List.map gen_category entry.categories)
	in
	let link = match feed.feed_link with
		| Some link	-> [ create ~ns "link" ~attr:[ "href", link ] [] ]
		| None		-> []
	and icon = match feed.feed_icon with
		| Some icon	-> [ create_text ~ns "icon" icon ]
		| None		-> []
	in
	create ~ns "feed" (
		create_text ~ns "title" feed.feed_title
		:: link
		@ icon
		@ List.map gen_entry (Array.to_list feed.entries))
