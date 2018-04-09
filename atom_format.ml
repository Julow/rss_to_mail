open Xml_utils
open Feed

let ns = namespace "http://www.w3.org/2005/Atom"

let parse feed_elem =
	let parse_entry entry =
		let child_text_opt tag =
			try Some (text (child ~ns tag entry))
			with _ -> None
		in
		let date =
			Int64.of_float @@
			Js.date##parse (node (child ~ns "updated" entry))##getText
		and authors =
			List.map (fun author -> text (child ~ns "name" author)) @@
			children ~ns "author" entry
		and categories =
			let parse_category cat =
				let attr_opt k = try Some (attribute k cat) with _ -> None in
				{ term = attr_opt "term"; label = attr_opt "label" }
			in
			children ~ns "category" entry |> List.map parse_category
		in
		{	id = child_text_opt "id";
			title = text (child ~ns "title" entry);
			link = attribute "href" (child ~ns "link" entry);
			summary = child_text_opt "summary";
			content = child_text_opt "content";
			authors; date; categories }
	in
	let feed_title = text (child ~ns "title" feed_elem)
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
	{ feed_title; feed_link; entries }

let generate feed =
	let gen_entry entry =
		let gen_category cat =
			let opt k = function Some v -> [ k, v ] | None -> [] in
			let attr = opt "term" cat.term @ opt "label" cat.label in
			create ~ns "category" ~attr []
		and gen_author name =
			create ~ns "author" [ create_text ~ns "name" name ]
		in
		let content = match entry.content with
			| Some content	->
				[ create_text ~ns "content" ~attr:[ "type", "html" ] content ]
			| None			-> []
		and summary = match entry.summary with
			| Some sum		->
				[ create_text ~ns "summary" ~attr:[ "type", "html" ] sum ]
			| None			-> []
		and id = match entry.id with
			| Some id		-> [ create_text ~ns "id" id ]
			| None			-> []
		in
		create ~ns "entry" ([]
			@ List.map gen_author entry.authors
			@ summary
			@ content
			@ id
			@ create_text ~ns "title" entry.title
			:: create ~ns "link" ~attr:[ "href", entry.link ] []
			:: create_text ~ns "updated" (entry_date_string entry)
			:: List.map gen_category entry.categories)
	in
	let link = match feed.feed_link with
		| Some link	-> [ create ~ns "link" ~attr:[ "href", link ] [] ]
		| None		-> []
	in
	create ~ns "feed" (
		[ create_text ~ns "title" feed.feed_title ] @
		link @
		List.map gen_entry (Array.to_list feed.entries))
