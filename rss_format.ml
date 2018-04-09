open Xml_utils
open Feed

let parse rss_elem =
	let dc_ns = namespace "http://purl.org/dc/elements/1.1/" in
	let parse_item item =
		let date =
			Int64.of_float @@
			Js.date##parse (node (child "pubDate" item))##getText
		and categories =
			let parse_category cat =
				{ term = None; label = text cat } in
			List.map parse_category (children "category" item)
		in
		{	id = text (child "guid" item);
			title = text (child "title" item);
			content = text (child "description" item);
			link = text (child "link" item);
			author = text (child ~ns:dc_ns "creator" item);
			date; categories }
	in
	let channel = child "channel" rss_elem in
	{	feed_title = text (child "title" channel);
		feed_link = (try Some (text (child "link" channel)) with _ -> None);
		entries = List.map parse_item (children "item" channel) }
