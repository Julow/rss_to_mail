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

	let update ~first_update ~now uri scraper options seen_ids =
		Async.bind (Fetch.fetch uri) begin function
			| Error e		-> Async.return (`Fetch_error e)
			| Ok contents	->
				let feed = Scraper.scrap scraper contents in
				Check_feed.process ~first_update ~now uri options seen_ids feed
		end

	let check ~now uri scraper options data =
		let first_update, uptodate, seen_ids =
			match data with
			| Some (last_update, seen_ids) ->
				let uptodate = Utils.is_uptodate now last_update options in
				false, uptodate, seen_ids
			| None -> true, false, SeenSet.empty
		in
		if uptodate then Async.return `Uptodate
		else update ~first_update ~now uri scraper options seen_ids

end
