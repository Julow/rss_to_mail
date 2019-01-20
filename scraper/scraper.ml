open Feed

type entry = Title | Link
type feed = Feed_title | Feed_icon | Entry of entry Scrap.t list

let text node = String.concat "" (Soup.texts node)

let scrap_entry node t = function
	| Title			->
		Feed.{ t with title = Some (text node) }
	| Link			->
		let link = Soup.attribute "href" node |> Option.map Uri.of_string in
		{ t with link }

let scrap_feed node (title, icon, entries) = function
	| Feed_title	-> Some (text node), icon, entries
	| Feed_icon		->
		let feed_icon = Soup.attribute "src" node |> Option.map Uri.of_string in
		title, feed_icon, entries
	| Entry s	->
		let e = List.fold_left (Scrap.scrap node scrap_entry) Feed.empty_entry s in
		title, icon, e :: entries

let scrap node s =
	let feed_title, feed_icon, entries =
		Scrap.scrap node scrap_feed (None, None, []) s in
	Feed.{	feed_title; feed_icon;
			feed_link = None;
			entries = Array.of_list entries }

type t = feed Scrap.t

(** Perform scraping *)
let scrap t source =
	scrap (source |> Soup.(require % child_element % parse)) t
