let print_refresh = function
	| `Every h		-> Printf.sprintf "(Every %f)" h
	| `At (h, m)	-> Printf.sprintf "(At %d %d)" h m

let check_is_uptodate expected now last_update refresh =
	let options = Feed_options.make ~refresh () in
	let desc =
		Printf.sprintf "now=%Ld last_update=%Ld refresh=%s"
			now last_update (print_refresh refresh)
	in
	Alcotest.(check bool) desc expected
		(Utils.is_uptodate now last_update options)

let hourly () =
	check_is_uptodate false 14401L 0L (`Every 4.);
	check_is_uptodate true 14399L 0L (`Every 4.);
	()

let daily () =
	check_is_uptodate true 12304799L 12345678L (`At (10, 0));
	check_is_uptodate true 12304801L 12345678L (`At (10, 0));
	check_is_uptodate true 12391199L 12345678L (`At (10, 0));
	check_is_uptodate false 12391201L 12345678L (`At (10, 0));
	()

let tests = [
	"Hourly refresh", `Quick, hourly;
	"Daily refresh", `Quick, daily
]
