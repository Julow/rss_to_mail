module Make (Async : sig
		type 'a t
		val return : 'a -> 'a t
		val bind : 'a t -> ('a -> 'b t) -> 'b t
	end)
	(Fetch : sig
		type error
		val fetch : Uri.t -> (string, error) result Async.t
	end) =
struct

	module Check_feed = Check_feed.Make (Async) (Fetch)

	let update ~first_update ~now url scraper options seen_ids =
		let uri = Uri.of_string url in
		Async.bind (Fetch.fetch uri) begin function
			| Error e		-> Async.return (`Fetch_error e)
			| Ok contents	->
				let feed = Scraper.scrap scraper contents in
				let feed, seen_ids, entries =
					Check_feed.process ~now uri options seen_ids feed
				in
				let mails =
					if first_update then []
					else
						let sender = Check_feed.sender_name uri feed options in
						List.map (Check_feed.prepare_mail ~sender feed options) entries
				in
				Async.return (`Ok ([ url, seen_ids ], mails))
		end

	let check ~now get_feed_data url scraper options =
		let first_update, uptodate, seen_ids =
			Check_feed.check_data ~now options (get_feed_data url)
		in
		if uptodate then Async.return `Uptodate
		else update ~first_update ~now url scraper options seen_ids

end
