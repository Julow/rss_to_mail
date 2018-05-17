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
