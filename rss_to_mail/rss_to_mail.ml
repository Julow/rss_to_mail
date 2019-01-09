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

	type nonrec mail = mail

	(**
	 * Check a feed for updates
	 * Returns the list of generated mails and updated feed datas
	 * Log informations by calling [log] once for each feed
	 *)
	let check ~now get_feed_data (feed, options) =
		match feed with
		| Feed_desc.Feed url	->
			let uri = Uri.of_string url in
			let data = get_feed_data url in
			Async.bind (Check_feed.check ~now uri options data) begin
				function
				| `Ok (seen_ids, mails) ->
					Async.return (mails,
						[ url, `Updated (seen_ids, List.length mails) ])
				| `Uptodate | `Fetch_error _ | `Parsing_error _ as r ->
					Async.return ([], [ url, r ])
			end

end
