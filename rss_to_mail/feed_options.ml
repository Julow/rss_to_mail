(** Per feed options *)

type t = {
	cache		: float; (** Cache timeout in hours *)
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
	| _				-> invalid_arg "Feed_options.cache_of_string"

let make ?(cache=6.) ?label ?(no_content=false) ?scraper () =
	{ cache; label; no_content; scraper }
