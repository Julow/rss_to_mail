open Script_API

type category = {
	label		: string;
	term		: string option
}

type entry = {
	id			: string;
	title		: string;
	author		: string option;
	categories	: category list;
	content		: string;
	link		: string;
	date		: Int64.t
}

type t = {
	feed_title	: string;
	feed_link	: string option;
	entries		: entry list
}

let entry_date_string t =
	let date = new%js Js.date_fromTimeValue (Int64.to_float t.date) in
	Js.to_string date##toLocaleString
