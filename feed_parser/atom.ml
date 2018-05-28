open Feed
open Xml
open Operators

(** Atom parser *)

let ns = "http://www.w3.org/2005/Atom"
and media_ns = "http://search.yahoo.com/mrss/"

let (<) ?(ns=ns) = (<) ~ns
let (<<) ?(ns=ns) = (<<) ~ns

module Links =
struct

	type rel =
		| Alternate
		| Self
		| Enclosure
		| Other of string

	type t = (rel * (string * Xml.node)) list

	let rel = Option.map_or Alternate @@
		function
		| "alternate"	-> Alternate
		| "self"		-> Self
		| "enclosure"	-> Enclosure
		| rel			-> Other rel

	let eq = Pervasives.(=)

	let get_all rel links = List.assoc_all ~eq rel links
	let get rel links = List.assoc_opt ~eq rel links

	let rec of_nodes =
		function
		| []			-> []
		| node :: tl	->
			match attr "href" node with
			| None			-> of_nodes tl
			| Some href		->
				let rel = rel (attr "rel" node) in
				(rel, (href, node)) :: of_nodes tl

end

let attachment (href, node) =
	{	attach_url = Uri.of_string href;
		attach_size = attr "length" node >$ Int64.of_string;
		attach_type = attr "type" node }

let author node =
	node < "name" > fun name ->
		{	author_name = text name;
			author_link = node < "uri" > Uri.of_string % text }

let category node = { term = attr "term" node; label = attr "label" node }

let entry node =
	let links = Links.of_nodes (children ~ns "link" node) in
	{	id = node < "id" > text;
		title = node < "title" > text;
		summary = node < "summary" > text;
		content = node < "content" > text;
		date = node < "updated" > text;
		link = Links.(get Alternate) links > Uri.of_string % fst;
		attachments = Links.(get_all Enclosure) links >> attachment;
		thumbnail = (<) ~ns:media_ns node "thumbnail" >$ attr "url" > Uri.of_string;
		authors = node << "author" >>$ author;
		categories = node << "category" >> category }

let feed node =
	let links = Links.of_nodes (children ~ns "link" node) in
	{	feed_title = node < "title" > text;
		feed_icon = node < "icon" > Uri.of_string % text;
		feed_link = Links.(get Alternate) links > Uri.of_string % fst;
		entries = node << "entry" >> entry |> Array.of_list }

let parse = feed
