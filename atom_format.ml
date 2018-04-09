open Xml_utils
open Feed

let ns = namespace "http://www.w3.org/2005/Atom"

let parse feed_elem =
	let parse_entry entry =
		let date =
			Int64.of_float @@
			Js.date##parse (node (child ~ns "updated" entry))##getText
		and author =
			try Some (text (child ~ns "name" (child ~ns "author" entry)))
			with _ -> None
		and categories =
			let parse_category cat =
				let term = try Some (attribute "term" cat) with _ -> None
				and label = try Some (attribute "label" cat) with _ -> None in
				{ label; term }
			in
			children ~ns "category" entry |> List.map parse_category
		in
		{	id = text (child ~ns "id" entry);
			title = text (child ~ns "title" entry);
			content = text (child ~ns "content" entry);
			link = attribute "href" (child ~ns "link" entry);
			author; date; categories }
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
		in
		let author = match entry.author with
			| Some author	->
				[ create ~ns "author" [ create_text ~ns "name" author ] ]
			| None			-> []
		in
		create ~ns "entry" (
			author
			@ create_text ~ns "title" entry.title
			:: create_text ~ns "content" ~attr:[ "type", "html" ] entry.content
			:: create_text ~ns "id" entry.id
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
