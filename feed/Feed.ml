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
