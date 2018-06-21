open Feed
open Feed_options

type mail = {
	sender		: string;
	subject		: string;
	body		: string
}

module Make (Async : sig
		type 'a t
		val return : 'a -> 'a t
		val bind : 'a t -> ('a -> 'b t) -> 'b t
	end)
	(Fetch : sig
		val fetch : Uri.t -> (string, int) result Async.t
	end) =
struct

	type nonrec mail = mail = {
		sender		: string;
		subject		: string;
		body		: string
	}

	(* Remove date for IDs that disapeared from the feed: 1 month *)
	let remove_date_from = Int64.(+) 2678400L

	let prepare_mail feed_uri feed options entry =
		let sender =
			match feed.feed_title with
			| Some title	-> title
			| None			->
				match Uri.host feed_uri with
				| Some host		-> host
				| None			-> Uri.to_string feed_uri
		in
		let subject =
			match entry.title with
			| Some title	-> title
			| None			-> "New entry from " ^ sender
		in
		let body = Mail_body.generate feed options entry in
		{ sender; subject; body }

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

	let process ~first_update ~now uri options seen_ids feed =
		let feed = Feed.resolve_urls uri feed in
		let seen_ids, entries =
			new_entries (remove_date_from now) seen_ids feed.entries in
		let mails =
			(* First update, don't send any mails *)
			if first_update then []
			else List.map (prepare_mail uri feed options) entries in
		Async.return (`Ok (seen_ids, mails))

	let update ~first_update ~now uri options seen_ids =
		let process_feed contents =
			match Feed_parser.parse (`String (0, contents)) with
			| exception Feed_parser.Error (pos, msg) ->
				Async.return (`Parsing_error (pos, msg))
			| feed ->
				process ~first_update ~now uri options seen_ids feed
		and process_scrap scraper contents =
			let feed = Scraper.scrap scraper contents in
			process ~first_update ~now uri options seen_ids feed
		in
		let f = Fetch.fetch uri in
		Async.bind f (function
		| Error code	-> Async.return (`Fetch_error code)
		| Ok contents	->
			match options.scraper with
			| Some sc		-> process_scrap sc contents
			| None			-> process_feed contents)

	let is_uptodate now last_update options =
		Int64.(last_update + of_float (options.cache *. 3600.) >= now)

	let check ~now uri options data =
		let first_update, uptodate, seen_ids =
			match data with
			| Some (last_update, seen_ids) ->
				let uptodate = is_uptodate now last_update options in
				false, uptodate, seen_ids
			| None -> true, false, SeenSet.empty
		in
		if uptodate then Async.return `Uptodate
		else update ~first_update ~now uri options seen_ids

end
