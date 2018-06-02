open Feed
open Feed_options

let send_mail to_ (sender, subject, body) =
	MailApp.t##sendEmail (object%js
		val to_ = to_
		val subject = Js.string subject
		val htmlBody = Js.string body
		val name = Js.string sender
	end)

let load_spreadsheet datas =
	match Properties.get_sheet_id datas with
	| Some id		-> Spreadsheet.read id
	| None			->
		let id = Spreadsheet.create () in
		Properties.set_sheet_id datas id;
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
	Int64.(last_update + of_float (options.cache *. 3600.) >= now)

let feed_data props url =
	Properties.get_feed_data props url |> Option.get (SeenSet.empty, 0L)

let fmt = Printf.sprintf

let update props =
	let user_email = Session.t##getActiveUser##getEmail in
	(if user_email##.length = 0 then failwith "Can't access user's email");
	let now = 0L in
	(* Remove date for IDs that disapeared from the feed: 1 month *)
	let remove_date = Int64.(+) now 2678400L in
	load_spreadsheet props
	|> List.filter_map (fun (url, options) ->
		let (seen_ids, last_update as data) = feed_data props url in
		if not (should_update now last_update options)
		then None
		else Some (Fetch.url url (function
			| Error code		-> Error (fmt "%s: Fetch error: %d" url code)
			| Ok contents		->
				let inp = Xmlm.make_input (`String (0, contents)) in
				match Feed_parser.parse inp with
				| exception Feed_parser.Error ((line, col), msg) ->
					Error (fmt "%s: Parsing error: %d:%d: %s" url line col msg)
				| feed ->
					let feed = Feed.resolve_urls (Uri.of_string url) feed in
					let seen_ids, entries =
						new_entries remove_date seen_ids feed.entries in
					let mails = List.map (prepare_mail url feed options) entries in
					Ok (url, (seen_ids, now), mails)
	)))
	|> List.last 5
	|> Fetch.perform
	|> List.iter (function
		| Ok (url, data, mails)	->
			Properties.set_feed_data props url data;
			Console.info (fmt "%d new entries from %s" (List.length mails) url);
			List.iter (send_mail user_email) mails
		| Error msg				->
			Console.error msg)

let do_update () =
	Console.t##time (Js.string "all");
	let props = Properties.load () in
	update props;
	Properties.save props;
	Console.t##timeEnd (Js.string "all")

let () = Js.export "rss_to_mail"
	(object%js
		method doUpdate _ = do_update ()
	end)
