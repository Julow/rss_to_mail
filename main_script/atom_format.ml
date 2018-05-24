open Xml_utils
open Feed

let ns = namespace "http://www.w3.org/2005/Atom"
let media_ns = namespace "http://search.yahoo.com/mrss/"

let parse feed_elem =
	let parse_entry entry =
		let date node =
			Int64.of_float @@ Js.date##parse (raw_text node)
		and authors =
			let parse_author author =
				let author_link =
					child_opt (Uri.of_string % text) ~ns "uri" author in
				{ author_name = text (child ~ns "name" author); author_link }
			in
			List.map parse_author @@ children ~ns "author" entry
		and categories =
			let parse_category cat =
				let attr_opt k = try Some (attribute k cat) with _ -> None in
				{ term = attr_opt "term"; label = attr_opt "label" }
			in
			List.map parse_category @@ children ~ns "category" entry
		and thumbnail =
			let f node = Uri.of_string (attribute "url" node) in
			child_opt f ~ns:media_ns "thumbnail" entry
		and link, attachments =
			let filter (alt, enc) l =
				match attribute "rel" l with
				| exception Attribute_not_found _ -> l :: alt, enc
				| "alternate"		-> l :: alt, enc
				| "enclosure"		-> alt, l :: enc
				| _					-> alt, enc
			in
			let alternates, enclosures = children ~ns "link" entry
				|> List.fold_left filter ([], []) in
			let link = match alternates with
				| []		-> None
				| l :: _	-> Some (Uri.of_string (attribute "href" l))
			and attachment e =
				{	attach_url = Uri.of_string (attribute "href" e);
					attach_size = attribute_opt Int64.of_string_exn "length" e;
					attach_type = attribute_opt (fun t -> t) "type" e }
			in
			link, List.map attachment enclosures
		in
		{	id = child_opt text ~ns "id" entry;
			title = child_opt text ~ns "title" entry;
			summary = child_opt text ~ns "summary" entry;
			content = child_opt raw_text ~ns "content" entry;
			date = child_opt date ~ns "updated" entry;
			link; attachments; thumbnail; authors; categories }
	in
	let feed_title = child_opt text ~ns "title" feed_elem
	and feed_icon = child_opt (Uri.of_string % text) ~ns "icon" feed_elem
	and feed_link =
		try
			let alternate link =
				try String.equal (attribute "rel" link) "alternate"
				with _ -> false in
			List.find alternate (children ~ns "link" feed_elem)
			|> fun link -> Some (Uri.of_string (attribute "href" link))
		with _ -> None
	and entries =
		Array.of_list (children ~ns "entry" feed_elem)
		|> Array.map parse_entry
	in
	{ feed_title; feed_link; feed_icon; entries }

let generate feed =
	let m t f = match t with Some v -> [ f v ] | None -> [] in
	let gen_entry entry =
		let gen_category cat =
			let attr =
				m cat.term (fun v -> "term", v)
				@ m cat.label (fun v -> "label", v)
			in
			create ~ns "category" ~attr []
		and gen_author t =
			let uri = m t.author_link (create_text ~ns "uri" % Uri.to_string)
			and name = create_text ~ns "name" t.author_name in
			create ~ns "author" (name :: uri)
		and gen_attachment t =
			let attr =
				("href", Uri.to_string t.attach_url)
				:: ("rel", "enclosure")
				:: m t.attach_size (fun s -> "length", Int64.to_string s)
				@ m t.attach_type (fun t -> "type", t)
			in
			create ~ns "link" ~attr []
		in
		(** option map to_list *)
		create ~ns "entry" ([]
			@ List.map gen_author entry.authors
			@ m entry.summary
				(create_text ~ns "summary" ~attr:[ "type", "html" ])
			@ m entry.content
				(create_raw_text ~ns "content" ~attr:[ "type", "html" ])
			@ m entry.id (create_text ~ns "id")
			@ m entry.link (fun link ->
				create ~ns "link" ~attr:[ "href", Uri.to_string link ] [])
			@ List.map gen_attachment entry.attachments
			@ m entry.thumbnail (fun url ->
				let url = Uri.to_string url in
				create ~ns:media_ns "thumbnail" ~attr:[ "url", url ] [])
			@ m entry.title (create_text ~ns "title")
			@ m entry.date (create_text ~ns "updated" % Utils.date_string)
			@ List.map gen_category entry.categories)
	in
	let link = match feed.feed_link with
		| Some link	->
			[ create ~ns "link" ~attr:[ "href", Uri.to_string link ] [] ]
		| None		-> []
	in
	create ~ns "feed" (
		m feed.feed_title (create_text ~ns "title")
		@ link
		@ m feed.feed_icon (create_text ~ns "icon" % Uri.to_string)
		@ List.map gen_entry (Array.to_list feed.entries))
