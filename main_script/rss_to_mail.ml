open Feed
open Feed_options

let send_mail to_ (sender, subject, body) =
	MailApp.t##sendEmail (object%js
		val to_ = to_
		val subject = Js.string subject
		val htmlBody = Js.string body
		val name = Js.string sender
	end)

(** Send a list of mail
	On error, stop and returns the list of unsent mails *)
let rec send_mails to_ = function
	| []			-> []
	| mail :: tl	->
		match send_mail to_ mail with
		| exception Js.Error _	-> mail :: tl
		| ()					-> send_mails to_ tl

let load_spreadsheet datas =
	match Properties.get_sheet_id datas with
	| Some id		-> Spreadsheet.read id
	| None			->
		let id = Spreadsheet.create () in
		Properties.set_sheet_id id;
		[]

let prepare_mail feed_url feed options entry =
	let sender =
		match feed.feed_title with
		| Some title	-> title
		| None			->
			match Uri.(host (of_string feed_url)) with
			| Some host		-> host
			| None			-> feed_url
	in
	let subject =
		match entry.title with
		| Some title	-> title
		| None			-> "New entry from " ^ sender
	in
	let body = Mail_body.generate feed options entry in
	sender, subject, body

let new_entries remove_date seen_ids entries =
	let ids, news = Array.fold_right (fun e (ids, news) ->
		match entry_id e with
		| Some id	->
			let news = if SeenSet.is_seen id seen_ids
					then news else e :: news in
			id :: ids, news
		| None		-> ids, news
	) entries ([], []) in
	SeenSet.new_ids remove_date ids seen_ids, news

let should_update now last_update options =
	Int64.(last_update + of_float (options.cache *. 3600.) < now)

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
	(* Remove date for IDs that disapeared from the feed: 1 month *)
	let remove_date = Int64.(+) now 2678400L in
	load_spreadsheet props
	|> List.filter_map (fun (url, options) ->
		let seen_ids, last_update = feed_data props url in
		if not (should_update now last_update options)
		then None
		else Some (Fetch.url url (function
			| Error code		-> Error (fmt "%s: Fetch error: %d" url code)
			| Ok contents		->
				Console.t##time (Js.string url);
				let inp = Xmlm.make_input (`String (0, contents)) in
				match Feed_parser.parse inp with
				| exception Feed_parser.Error ((line, col), msg) ->
					Error (fmt "%s: Parsing error: %d:%d: %s" url line col msg)
				| feed ->
					let feed = Feed.resolve_urls (Uri.of_string url) feed in
					let seen_ids, entries =
						new_entries remove_date seen_ids feed.entries in
					let mails = List.map (prepare_mail url feed options) entries in
					let mails =
						(* First update, don't send any mails *)
						if Int64.(=) last_update 0L then [] else mails in
					Console.t##timeEnd (Js.string url);
					Console.info (fmt "%d new entries from %s"
							(List.length mails) url);
					Ok (url, (seen_ids, now), mails)
	)))
	|> tap (Console.info % fmt "%d feeds to update" % List.length)
	|> List.(take 5 % shuffle) (* Process only 5 feeds (choosen randomly) *)
	|> Fetch.perform
	|> List.fold_left (fun mails -> function
		| Ok (url, data, mails')	->
			(try Properties.set_feed_data url data
			with _ -> Console.error ("Failed to set the properties for " ^ url));
			mails' @ mails
		| Error msg			-> Console.error msg; mails)
		[]
	|> (fun mails ->
		Properties.get_unsent_mails props
		|> flip (@) mails
		|> send_mails (get_user_email ())
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
