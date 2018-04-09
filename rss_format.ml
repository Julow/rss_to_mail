open Xml_utils
open Feed

let parse rss_elem =
	let dc_ns = namespace "http://purl.org/dc/elements/1.1/"
	and content_ns = namespace "http://purl.org/rss/1.0/modules/content/" in
	let parse_item item =
		let child_text_opt ?ns tag =
			try Some (text (child ?ns tag item))
			with _ -> None
		in
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
		{	title = text (child "title" item);
			link = child_text_opt "link";
			id = child_text_opt "guid";
			summary = child_text_opt "description";
			content = child_text_opt ~ns:content_ns "encoded";
			authors; date; categories }
	in
	let channel = child "channel" rss_elem in
	let entries =
		Array.of_list (children "item" channel)
		|> Array.map parse_item
	in
	{	feed_title = text (child "title" channel);
		feed_link = (try Some (text (child "link" channel)) with _ -> None);
		entries }
