let send_mail to_ mail =
	let open Rss_to_mail in
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

let feed_data props url =
	Properties.get_feed_data props url |> Option.get (SeenSet.empty, 0L)

let fmt = Printf.sprintf

let get_user_email () =
	let email = Session.t##getActiveUser##getEmail in
	(if email##.length = 0 then failwith "Can't access user's email");
	email

let date_now () =
	Int64.of_float ((new%js Js.date_now)##getTime /. 1000.)

let update () =
	let props = Properties.load () in
	let now = date_now () in
	load_spreadsheet props
	|> List.filter_map (fun (url, options) ->
		let seen_ids, last_update = feed_data props url in
		if not (Rss_to_mail.should_update now last_update options)
		then None
		else Some (Fetch.url url (function
			| Error code		-> url, `Fetch_error code
			| Ok contents		->
				Console.t##time (Js.string url);
				let first_update = Int64.(=) last_update 0L
				and uri = Uri.of_string url
				and source = `String (0, contents) in
				let r = Rss_to_mail.update ~first_update ~now
						uri options seen_ids source in
				Console.t##timeEnd (Js.string url);
				url, r
	)))
	|> tap (Console.info % fmt "%d feeds to update" % List.length)
	|> List.(take 5 % shuffle) (* Process only 5 feeds (choosen randomly) *)
	|> Fetch.perform
	|> List.fold_left (fun mails -> function
		| url, `Ok (seen_ids, mails')	->
			Console.info (fmt "%d new entries from %s"
					(List.length mails') url);
			(try Properties.set_feed_data url (seen_ids, now)
			with _ -> Console.error ("Failed to set the properties for " ^ url));
			mails' @ mails
		| url, `Fetch_error code		->
			Console.error (fmt "%s: Fetch error (%d)" url code);
			mails
		| url, `Parsing_error ((line, col), msg) ->
			Console.error (fmt "%s: Parsing error: %d:%d: %s" url line col msg);
			mails)
		[]
	|> (fun mails ->
		Properties.get_unsent_mails props
		|> flip (@) mails
		|> send_mails (lazy (get_user_email ()))
		|> Properties.set_unsent_mails)

let do_update () =
	Random.self_init ();
	Console.t##time (Js.string "all");
	update ();
	Console.t##timeEnd (Js.string "all")

let () = Js.export "rss_to_mail"
	(object%js
		method doUpdate _ = do_update ()
	end)
