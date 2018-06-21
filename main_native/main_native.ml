(** Ensures [f] is running at most [n] times concurrently
	Internally uses an Lwt_pool of [unit] *)
let pooled n f =
	let pool = Lwt_pool.create n (const Lwt.return_unit) in
	fun x -> Lwt_pool.use pool (fun () -> f x)

module Fetch =
struct

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

end

module Rss_to_mail = Rss_to_mail.Make (Lwt) (Fetch)

let check_feeds ~now feed_datas feeds =
	let check_feed (url, options) =
		let uri = Uri.of_string url
		and data = StringMap.get url feed_datas in
		let%lwt r = Rss_to_mail.check ~now uri options data in
		Lwt.return (url, r)
	and handle (feed_datas, mails as acc) = function
		| url, `Fetch_error code		->
			eprintf "%s: Fetch error: %d\n" url code; acc
		| url, `Parsing_error ((line, col), msg) ->
			eprintf "%s: Parsing error: %d:%d: %s\n" url line col msg; acc
		| url, `Uptodate				->
			printf "%s: Uptodate\n" url;
			acc
		| url, `Ok (seen_ids, mails')	->
			printf "%s: %d new entries\n" url (List.length mails');
			StringMap.add url (now, seen_ids) feed_datas,
			mails' @ mails
	in
	(** Done in 2 passes to improve concurrency *)
	Lwt_list.map_p check_feed feeds
	|> Lwt.map (List.fold_left handle (feed_datas, []))

(** Send a list of mail to [to_]
	Returns the list of unsent emails
	(if for any reason, swaks returned an error status) *)
let send_mails (server, auth) to_ mails =
	let send (t : Rss_to_mail.mail) =
		Swaks.send_mail ~server ?auth ~from:t.sender ~to_ t.subject t.body
		|> Lwt.map (function `Ok -> None | _ -> Some t)
	in
	(** At most 2 mails sending in parallel *)
	Lwt_list.filter_map_p (pooled 2 send) mails

let with_feed_datas config_file f =
	match Persistent_data.load_feeds config_file with
	| exception Failure msg ->
		failwith (sprintf "Error: %s: %s\n" config_file msg)
	| config	->
		let datas = Persistent_data.load_feed_datas () in
		let%lwt datas = f config datas in
		Persistent_data.save_feed_datas datas;
		Lwt.return_unit

let run (conf : Persistent_data.config) (feed_datas, unsent) =
	let now = Unix.time () |> Int64.of_float in
	let%lwt feed_datas, mails = check_feeds ~now feed_datas conf.feeds in
	printf "%d new entries\n" (List.length mails);
	let%lwt unsent = send_mails conf.smtp conf.address (unsent @ mails) in
	(if not (List.is_empty unsent) then
		printf "%d mails could not be sent\n" (List.length unsent));
	Lwt.return (feed_datas, unsent)

let () =
	let config_file = try Sys.argv.(1) with _ -> "feeds.sexp" in
	Lwt_main.run (with_feed_datas config_file run)
