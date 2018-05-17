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

type entry = {
	id			: string option;
	title		: string;
	authors		: author list;
	categories	: category list;
	summary		: string option;
	content		: Js.js_string Js.t option;
	link		: Uri.t option;
	thumbnail	: Uri.t option;
	date		: Int64.t;
	attachments	: attachment list
}

type t = {
	feed_title	: string;
	feed_link	: Uri.t option;
	feed_icon	: Uri.t option;
	entries		: entry array
}

let entry_date_string t =
	let date = new%js Js.date_fromTimeValue (Int64.to_float t.date) in
	Js.to_string date##toLocaleString

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
