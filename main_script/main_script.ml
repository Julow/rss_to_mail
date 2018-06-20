let send_mail to_ (mail : Rss_to_mail.mail) =
	MailApp.t##sendEmail (object%js
		val to_ = to_
		val subject = Js.string mail.subject
		val htmlBody = Js.string mail.body
		val name = Js.string mail.sender
	end)

(** Send a list of mail
	On error, stop and returns the list of unsent mails *)
let rec send_mails to_ = function
	| []			-> []
	| mail :: tl	->
		match send_mail (Lazy.force to_) mail with
		| exception Js.Error _	-> mail :: tl
		| ()					-> send_mails to_ tl

let load_spreadsheet datas =
	match Properties.get_sheet_id datas with
	| Some id		-> Spreadsheet.read id
	| None			->
		let id = Spreadsheet.create () in
		Properties.set_sheet_id id;
		[]

let fmt = Printf.sprintf

let get_user_email () =
	let email = Session.t##getActiveUser##getEmail in
	(if email##.length = 0 then failwith "Can't access user's email");
	email

let date_now () =
	Int64.of_float ((new%js Js.date_now)##getTime /. 1000.)

let fetch uri =
	let uri = Js.string (Uri.to_string uri) in
	try
		let res = UrlFetchApp.t##fetch_param uri (object%js
				val muteHttpExceptions = Js._true
			end) in
		let code = res##getResponseCode in
		if code = 200
		then Ok (Js.to_string res##getContentText)
		else Error code
	with _ -> Error ~-1

module Fetch =
struct

	let fetch uri =
		let uri = Js.string (Uri.to_string uri) in
		try
			let res = UrlFetchApp.t##fetch_param uri (object%js
					val muteHttpExceptions = Js._true
				end) in
			let code = res##getResponseCode in
			if code = 200
			then Ok (Js.to_string res##getContentText)
			else Error code
		with _ -> Error ~-1

end

module No_async =
struct
	type 'a t = 'a
	let return x = x
	let bind x f = f x
end

module Rss_to_mail = Rss_to_mail.Make (No_async) (Fetch)

let check_feed ~now ~props mails (url, options) =
	let uri = Uri.of_string url
	and data = Properties.get_feed_data props url in
	match Rss_to_mail.check ~now uri options data with
	| `Fetch_error code			->
		Console.error (fmt "%s: Fetch error (%d)" url code);
		mails
	| `Parsing_error ((line, col), msg) ->
		Console.error (fmt "%s: Parsing error: %d:%d: %s" url line col msg);
		mails
	| `Uptodate					-> mails
	| `Ok (seen_ids, mails')	->
		Console.info (fmt "%d new entries from %s" (List.length mails') url);
		(try Properties.set_feed_data url (now, seen_ids)
		with _ -> Console.error ("Failed to set the properties for " ^ url));
		mails' @ mails

let update () =
	let props = Properties.load () in
	let now = date_now () in
	let feeds = load_spreadsheet props in
	let unsent = Properties.get_unsent_mails props in
	let mails = List.fold_left (check_feed ~now ~props) [] feeds in
	let unsent = send_mails (lazy (get_user_email ())) (unsent @ mails) in
	(if not (List.is_empty unsent) then
		Console.info (fmt "%d mails could not be sent\n" (List.length unsent)));
	Properties.set_unsent_mails unsent

let () = Js.export "rss_to_mail"
	(object%js
		method doUpdate _ =
			Console.t##time (Js.string "all");
			update ();
			Console.t##timeEnd (Js.string "all")
	end)
