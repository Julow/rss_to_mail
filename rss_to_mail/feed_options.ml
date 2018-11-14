(** Per feed options *)

type t = {
	refresh		: [ `Every of float | `At of int * int ];
	(** Update interval, Every in hour or At a specific time in the day *)
	title		: string option; (** Override the feed's title *)
	label		: string option; (** Appended to the content *)
	no_content	: bool; (** If the content should be removed *)
	scraper		: Scraper.t option (** Used instead of Feed_parser *)
}

let make ?(refresh=`Every 6.) ?title ?label ?(no_content=false) ?scraper () =
	{ refresh; title; label; no_content; scraper }
