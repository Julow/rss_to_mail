(** Ensures [f] is running at most [n] times concurrently
	Internally uses an Lwt_pool of [unit] *)
let pooled n f =
	let pool = Lwt_pool.create n (const Lwt.return_unit) in
	fun x -> Lwt_pool.use pool (fun () -> f x)

let fetch url =
	let%lwt resp, body = Cohttp_lwt_unix.Client.call `GET url in
	let code = Cohttp.Code.code_of_status resp.status in
	if code <> 200
	then Lwt.return (Error code)
	else
		let%lwt body = Cohttp_lwt.Body.to_string body in
		Lwt.return (Ok body)

(** at most 5 fetch at once *)
let fetch = pooled 5 fetch

let update_feed ~first_update ~now seen_ids url options =
	let uri = Uri.of_string url in
	fetch uri
	|> Lwt.map (function
	| Error code	-> url, `Fetch_error code
	| Ok contents	->
		let source = `String (0, contents) in
		url,
		Rss_to_mail.update ~first_update ~now uri options seen_ids source)

let check_feed ~now feed_datas (url, options) =
	let first_update, (last_update, seen_ids) =
		match StringMap.get url feed_datas with
		| Some data		-> false, data
		| None			-> true, (0L, SeenSet.empty)
	in
	if not (Rss_to_mail.should_update now last_update options)
	then Lwt.return (url, `Uptodate)
	else update_feed ~first_update ~now seen_ids url options

let check_feeds ~now feed_datas feeds =
	let f (feed_datas, mails as acc) = function
		| url, `Fetch_error code		->
			Printf.eprintf "%s: Fetch error: %d" url code; acc
		| url, `Parsing_error ((line, col), msg) ->
			Printf.eprintf "%s: Parsing error: %d:%d: %s" url line col msg; acc
		| url, `Uptodate				-> acc
		| url, `Ok (seen_ids, mails')	->
			StringMap.add url (now, seen_ids) feed_datas,
			mails' @ mails
	in
	feeds
	|> Lwt_list.map_p (check_feed ~now feed_datas)
	|> Lwt.map (List.fold_left f (feed_datas, []))

(** Send a list of mail to [to_]
	Returns the list of unsent emails
	(if for any reason, swaks returned an error status) *)
let send_mails ~server ?tls ?auth ~to_ mails =
	let send t =
		let open Rss_to_mail in
		Swaks.send_mail ~server ?tls ?auth ~from:t.sender ~to_ t.subject t.body
		|> Lwt.map (function `Ok -> None | _ -> Some t)
	in
	(** At most 2 mails sending in parallel *)
	Lwt_list.filter_map_p (pooled 2 send) mails

let send_mails =
	match Sys.argv with
	| [| _; server; user; pass; to_ |] ->
		let auth = `Plain (user, pass) in
		send_mails ~server ~auth ~to_
	| _ -> failwith "Expecting 4 arguments: server, user, pass, address"

let run =
	let feeds = Persistent_data.load_feeds ()
	and feed_datas, unsent = Persistent_data.load_feed_datas ()
	and now = Unix.time () |> Int64.of_float in
	let%lwt feed_datas, mails = check_feeds ~now feed_datas feeds in
	Printf.printf "%d new entries\n" (List.length mails);
	let%lwt unsent = send_mails (unsent @ mails) in
	(if not (List.is_empty unsent) then
		Printf.printf "%d mails could not be sent\n" (List.length unsent));
	Persistent_data.save_feed_datas (feed_datas, unsent);
	Lwt.return ()

let () = Lwt_main.run run
