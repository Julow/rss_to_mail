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

	let prepare_bundle ~sender feed options entries =
		let len = List.length entries in
		if len = 0
		then []
		else
			let subject = string_of_int len ^ " entries from " ^ sender in
			let hidden_summary =
				List.filter_map (fun e -> e.Feed.title) entries
				|> String.concat ", "
				|> Mail_body.gen_summary in
			let label = options.Feed_desc.label in
			let body =
				List.map (Mail_body.gen_entry ~sender ?label feed) entries
				|> Mail_body.gen_mail ~sender ~hidden_summary
				|> Format.sprintf "%a" (Tyxml.Html.pp ()) in
			[ Utils.{ sender; subject; body } ]

	let update ~first_update ~now uri options seen_ids =
		Async.bind (Check_feed.fetch_feed uri) begin function
			| Error e		-> Async.return e
			| Ok feed		->
				let feed, seen_ids, entries =
					Check_feed.process ~now uri options seen_ids feed
				in
				let mails =
					if first_update then []
					else
						let sender = Check_feed.sender_name uri feed options in
						prepare_bundle ~sender feed options entries
				in
				Async.return (`Ok (seen_ids, mails))
		end

	let check ~now uri options data =
		let first_update, uptodate, seen_ids =
			match data with
			| Some (last_update, seen_ids) ->
				let uptodate = Utils.is_uptodate now last_update options in
				false, uptodate, seen_ids
			| None -> true, false, SeenSet.empty
		in
		if uptodate then Async.return `Uptodate
		else update ~first_update ~now uri options seen_ids

end
