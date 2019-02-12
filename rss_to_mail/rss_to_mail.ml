type mail = Utils.mail = {
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
		type error
		val fetch : Uri.t -> (string, error) result Async.t
	end) =
struct

	module Check_feed = Check_feed.Make (Async) (Fetch)
	module Check_scraper = Check_scraper.Make (Async) (Fetch)
	module Check_bundle = Check_bundle.Make (Async) (Fetch)

	type nonrec mail = mail

	(**
	 * Check a feed for updates
	 * Returns the list of generated mails and updated feed datas
	 * Log informations by calling [log] once for each feed
	 *)
	let check ~now get_feed_data (feed, options) =
		let updated = function
			| `Ok (updates, mails) ->
				let updates = List.map (SeenSet.filter_removed now) in
				Async.return (`Ok (updates, mails))
			| `Uptodate | `Fetch_error _ | `Parsing_error _ as r ->
				Async.return ([], [ url, r ])
		in
		let r =
			match feed with
			| Feed_desc.Feed url		->
				Check_feed.check ~now get_feed_data url options
			| Scraper (url, scraper)	->
				Check_scraper.check ~now get_feed_data url scraper options
			| Bundle url				->
				Check_bundle.check ~now get_feed_data url options
		in
		Async.bind r updated

end
