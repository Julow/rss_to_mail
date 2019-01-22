module No_async =
struct
	type 'a t = 'a
	let return x = x
	let bind t f = f t
end

(* Fetch feeds in the `feeds/` subdirectory
	Fetchs are blocking *)
module Local_fetch =
struct

	type error = int

	let fetch uri =
		let f = "feeds/" ^ Uri.to_string uri in
		eprintf "opening %s\n" f;
		No_async.return @@
		match open_in f with
		| exception Sys_error _		-> Error 404
		| inp						->
			let len = in_channel_length inp in
			Ok (really_input_string inp len)

end

module Rss_to_mail = Rss_to_mail.Make (No_async) (Local_fetch)

let now = 12345678L

open Printf

let print_mail (m : Rss_to_mail.mail) =
	printf "FROM: %s\nSUBJECT: %s\nBODY: %s\n" m.sender m.subject m.body

let print_options (opts : Feed_options.t) =
	printf "Options:";
	(match opts.refresh with
		| `Every h		-> printf " (refresh %f)" h
		| `At (h, m)	-> printf " (refresh (at %d:%d))" h m
		| `At_weekly (d, h, m) -> printf " (refresh (at %d:%d %d)" h m (CalendarLib.Date.int_of_day d));
	Option.iter (printf " (title %s)") opts.title;
	Option.iter (printf " (label %s)") opts.label;
	(if opts.no_content then printf " (no_content true)");
	printf "\n"

let print_data (_, r) =
	match r with
	| `Fetch_error code					->
		printf "Fetch error %d" code
	| `Parsing_error ((line, col), msg)	->
		printf "Parsing error (line %d, col %d)\n%s\n" line col msg
	| `Uptodate							->
		printf "Up-to-date\n"
	| `Updated _						->
		()

let check_feed feed_datas (feed, options) =
	begin match feed with
		| Feed_desc.Feed url	-> printf "\n# %s\n\n" url
		| Scraper (url, _)		-> printf "\n# scraper %s\n\n" url
		| Bundle url			-> printf "\n# bundle %s\n\n" url
	end;
	let datas url = StringMap.get url feed_datas in
	print_options options;
	let mails, datas = Rss_to_mail.check ~now datas (feed, options) in
	List.iter print_data datas;
	List.iter print_mail mails

let () =
	let feed_datas, _ = Persistent_data.load_feed_datas "feed_datas.sexp" in
	let data = Persistent_data.load_feeds "feeds.sexp" in
	List.iter (check_feed feed_datas) data.Persistent_data.feeds
