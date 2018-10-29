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

let check_feed (url, options) =
	printf "\n# %s\n\n" url;
	let datas = Some (0L, SeenSet.empty) in
	match Rss_to_mail.check ~now (Uri.of_string url) options datas with
	| `Fetch_error code					->
		printf "Fetch error %d\n" code
	| `Parsing_error ((line, col), msg)	->
		printf "Parsing error (line %d, col %d)\n%s\n" line col msg
	| `Uptodate							->
		printf "Up-to-date\n"
	| `Ok (_, mails)					->
		List.iter print_mail mails

let () =
	let data = Persistent_data.load_feeds "feeds.sexp" in
	List.iter check_feed data.Persistent_data.feeds
