open Feed

let t =
	let open Scrap in
	let open Scraper in
	R [
		"#main-content ol li", R [
			"strong a", T [ Entry (T [ Title; Link ]) ];
			"ol li a", T [ Entry (T [ Title; Link ]) ]
		]
	]

let test file =
	let feed = Scraper.scrap t (Soup.read_file file) in
	let feed = Feed.resolve_urls (Uri.of_string file) feed in
	Array.iter (fun entry ->
		let title = Option.get "<None>" entry.title
		and link = match entry.link with
			| Some l	-> Uri.to_string l
			| None		-> "<None>"
		in
		Printf.printf "%s: %s\n" title link
	) feed.entries

let () =
	for i = 1 to Array.length Sys.argv - 1 do
		test Sys.argv.(i)
	done
