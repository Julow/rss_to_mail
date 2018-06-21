type category = {
	label		: string option;
	term		: string option
}

type author = {
	author_name	: string;
	author_link	: Uri.t option
}

type attachment = {
	attach_url	: Uri.t;
	attach_size	: Int64.t option;
	attach_type	: string option
}

type 'a entry = {
	id			: string option;
	title		: string option;
	authors		: author list;
	categories	: category list;
	summary		: string option;
	content		: 'a option;
	link		: Uri.t option;
	thumbnail	: Uri.t option;
	date		: string option;
	attachments	: attachment list
}

type 'a t = {
	feed_title	: string option;
	feed_link	: Uri.t option;
	feed_icon	: Uri.t option;
	entries		: 'a entry array
}

let empty_entry = {
	id			= None;
	title		= None;
	authors		= [];
	categories	= [];
	summary		= None;
	content		= None;
	link		= None;
	thumbnail	= None;
	date		= None;
	attachments	= []
}

(** Try to returns the ID of an entry
	If the ID is None, try the link, title and date *)
let entry_id e =
	match e.id with
	| Some _ as id	-> id
	| None			->
		match e.link, e.title with
		| Some link, _				-> Some (Uri.to_string link)
		| None, (Some _ as title)	-> title
		| None, None				-> e.date

let resolve_urls feed_urls t =
	let res = Uri.resolve "" feed_urls in
	let res' = Option.map res in
	let author t = { t with author_link = res' t.author_link }
	and attachment t = { t with attach_url = res t.attach_url } in
	let entry t =
		{ t with
			authors = List.map author t.authors;
			link = res' t.link;
			thumbnail = res' t.thumbnail;
			attachments = List.map attachment t.attachments }
	in
	{ t with
		feed_link = res' t.feed_link;
		feed_icon = res' t.feed_icon;
		entries = Array.map entry t.entries }
