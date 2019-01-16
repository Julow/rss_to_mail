open Feed
open Xml
open Operators

(** RSS 2.0 parser
  * https://validator.w3.org/feed/docs/rss2.html *)

let dc_ns = "http://purl.org/dc/elements/1.1/"
and content_ns = "http://purl.org/rss/1.0/modules/content/"

let content_encoded content =
	Some (Html (Html_content.of_string content))

let author author_name = { author_name; author_link = None }
let category label = { term = None; label = Some label }

let attachment node =
	attr "url" node > fun url ->
		{	attach_url = Uri.of_string url;
			attach_size = attr "length" node >$ Int64.of_string;
			attach_type = attr "type" node }

let entry node =
	{	title	= node < "title" > text;
		link	= node < "link" > Uri.of_string % text;
		id		= node < "guid" > text;
		summary	= node < "description" > text >$ content_encoded;
		content	= (<) node ~ns:content_ns "encoded" > text >$ content_encoded;
		categories = node << "category" >> category % text;
		authors	= (<<) node ~ns:dc_ns "creator" >> author % text;
		attachments = node << "enclosure" >>$ attachment;
		date	= node < "pubDate" > text;
		thumbnail = None }

let feed node =
	{	feed_title	= node < "title" > text;
		feed_link	= node < "link" > Uri.of_string % text;
		feed_icon	= node < "image" >$ child "url" > Uri.of_string % text;
		entries		= node << "item" >>	entry |> Array.of_list }

let parse node =
	feed @@ child_exn "channel" node
