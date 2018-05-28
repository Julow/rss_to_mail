open Feed

let ns h = XmlService.t##getNamespace (Js.string h)

let atom_ns = ns "http://www.w3.org/2005/Atom"
and media_ns = ns "http://search.yahoo.com/mrss/"

let create ?(ns=atom_ns) tag ?(attr=[]) childs =
	let elem = XmlService.t##createElement_ns (Js.string tag) ns in
	let set_attr (k, v) =
		ignore (elem##setAttribute (Js.string k) (Js.string v))
	and add_child child =
		ignore (elem##addContent child)
	in
	List.iter set_attr attr;
	List.iter add_child childs;
	elem

let create_text ?ns tag ?attr text =
	let elem = create ?ns tag ?attr [] in
	ignore (elem##setText (Js.string text));
	elem

let generate feed =
	let m t f = match t with Some v -> [ f v ] | None -> [] in
	let gen_entry entry =
		let gen_category cat =
			let attr =
				m cat.term (fun v -> "term", v)
				@ m cat.label (fun v -> "label", v)
			in
			create "category" ~attr []
		and gen_author t =
			let uri = m t.author_link (create_text "uri" % Uri.to_string)
			and name = create_text "name" t.author_name in
			create "author" (name :: uri)
		and gen_attachment t =
			let attr =
				("href", Uri.to_string t.attach_url)
				:: ("rel", "enclosure")
				:: m t.attach_size (fun s -> "length", Int64.to_string s)
				@ m t.attach_type (fun t -> "type", t)
			in
			create "link" ~attr []
		in
		create "entry" ([]
			@ List.map gen_author entry.authors
			@ m entry.summary
				(create_text "summary" ~attr:[ "type", "html" ])
			@ m entry.content
				(create_text "content" ~attr:[ "type", "html" ])
			@ m entry.id (create_text "id")
			@ m entry.link (fun link ->
				create "link" ~attr:[ "href", Uri.to_string link ] [])
			@ List.map gen_attachment entry.attachments
			@ m entry.thumbnail (fun url ->
				let url = Uri.to_string url in
				create ~ns:media_ns "thumbnail" ~attr:[ "url", url ] [])
			@ m entry.title (create_text "title")
			@ m entry.date (create_text "updated")
			@ List.map gen_category entry.categories)
	in
	let link = match feed.feed_link with
		| Some link	->
			[ create "link" ~attr:[ "href", Uri.to_string link ] [] ]
		| None		-> []
	in
	create "feed" (
		m feed.feed_title (create_text "title")
		@ link
		@ m feed.feed_icon (create_text "icon" % Uri.to_string)
		@ List.map gen_entry (Array.to_list feed.entries))
