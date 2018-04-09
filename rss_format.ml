open Xml_utils
open Feed

let parse rss_elem =
	let dc_ns = namespace "http://purl.org/dc/elements/1.1/" in
	let child_text_opt ?ns tag node =
		try Some (text (child ?ns tag node))
		with _ -> None
	in
	let parse_item item =
		let date =
			Int64.of_float @@
			Js.date##parse (node (child "pubDate" item))##getText
		and categories =
			let parse_category cat =
				{ term = None; label = Some (text cat) } in
			List.map parse_category (children "category" item)
		in
		{	id = text (child "guid" item);
			title = text (child "title" item);
			link = text (child "link" item);
			author = child_text_opt ~ns:dc_ns "creator" item;
			summary = None;
			content = child_text_opt "description" item;
			date; categories }
	in
	let channel = child "channel" rss_elem in
	let entries =
		Array.of_list (children "item" channel)
		|> Array.map parse_item
	in
	{	feed_title = text (child "title" channel);
		feed_link = (try Some (text (child "link" channel)) with _ -> None);
		entries }
