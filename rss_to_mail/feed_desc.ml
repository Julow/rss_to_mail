type refresh_options = [
  | `Every of float (** Every [n] hours *)
  | `At of int * int (** Every days at [hour, min] *)
  | `At_weekly of CalendarLib.Date.day * int * int
  (** Every weeks on [day, hour, minute]*)
]

type options = {
  refresh		: refresh_options; (** Update interval *)
  title		: string option; (** Override the feed's title *)
  label		: string option; (** Appended to the content *)
  no_content	: bool; (** If the content should be removed *)
  filter		: (Str.regexp * bool) list; (** Filter entries by regex
                                          		The boolean is the expected result of [string_match] *)
}

let make_options ?(refresh=`Every 6.) ?title ?label ?(no_content=false)
    ?(filter=[]) () =
  { refresh; title; label; no_content; filter }

type desc =
  | Feed of string
  | Scraper of string * Scraper.t
  | Bundle of string

type t = desc * options
