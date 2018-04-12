open Xml_utils
open Feed

let ($) f g x = f (g x)

let parse rss_elem =
	let dc_ns = namespace "http://purl.org/dc/elements/1.1/"
	and content_ns = namespace "http://purl.org/rss/1.0/modules/content/" in
	let parse_item item =
		let date =
			Int64.of_float @@
			Js.date##parse (node (child "pubDate" item))##getText
		and categories =
			let parse_category cat =
				{ term = None; label = Some (text cat) } in
			List.map parse_category (children "category" item)
		and authors =
			let author creator =
				{ author_name = text creator; author_link = None }
			in
			List.map author (children ~ns:dc_ns "creator" item)
		in
		let raw_text n = (node n)##getText in
		{	title = text (child "title" item);
			link = child_opt text "link" item;
			id = child_opt text "guid" item;
			summary = child_opt text "description" item;
			content = child_opt raw_text ~ns:content_ns "encoded" item;
			thumbnail = None;
			authors; date; categories }
	in
	let channel = child "channel" rss_elem in
	let entries =
		Array.of_list (children "item" channel)
		|> Array.map parse_item
	in
	{	feed_title = text (child "title" channel);
		feed_link = child_opt text "link" channel;
		feed_icon = child_opt (text $ child "url") "image" channel;
		entries }
