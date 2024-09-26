type weekday =
  [ `Mon
  | `Tue
  | `Wed
  | `Thu
  | `Fri
  | `Sat
  | `Sun
  ]

type refresh_options =
  [ `Every of float  (** Every [n] hours *)
  | `At of int * int  (** Every days at [hour, min] *)
  | `At_weekly of weekday * int * int  (** Every weeks on [day, hour, minute]*)
  ]

type filter_expr =
  | And of filter_expr list
  | Or of filter_expr list
  | Not of filter_expr
  | Match_title of Str.regexp
  | Match_content of Str.regexp
      (** Matches both the summary and the full content. *)

type options = {
  refresh : refresh_options;  (** Update interval *)
  title : string option;  (** Override the feed's title *)
  label : string option;  (** Appended to the content *)
  content : [ `Keep | `Remove ];
      (** Whether to keep or remove the content and summary *)
  filter : filter_expr option;  (** Filter entries *)
  to_ : string option;  (** Destination email address *)
  max_entries : int option;  (** Max number of entries at the same time *)
}

let make_options ?(refresh = `Every 6.) ?title ?label ?(content = `Keep) ?filter
    ?to_ ?max_entries () =
  { refresh; title; label; content; filter; to_; max_entries }

type regular_feed =
  [ `Feed of string
  | `Scraper of string * Scraper.t
  ]

type desc =
  [ regular_feed
  | `Bundle of regular_feed
  | `Diff of string
  ]

type t = desc * options

let url_of_regular_feed = function `Feed url | `Scraper (url, _) -> url
