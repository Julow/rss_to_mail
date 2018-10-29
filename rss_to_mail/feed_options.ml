(** Per feed options *)

type t = {
	cache		: float; (** Cache timeout in hours *)
	title		: string option; (** Override the feed's title *)
	label		: string option; (** Appended to the content *)
	no_content	: bool; (** If the content should be removed *)
	scraper		: Scraper.t option (** Used instead of Feed_parser *)
}

let cache_of_string =
	function
	| "always"		-> 0.2
	| "often"		-> 1.5
	| "sometimes"	-> 6.
	| "daily"		-> 24.
	| "rarely"		-> 72.
	| s				->
		try float_of_string s
		with _ -> invalid_arg "Feed_options.cache_of_string"

let make ?(cache=6.) ?title ?label ?(no_content=false) ?scraper () =
	{ cache; title; label; no_content; scraper }
