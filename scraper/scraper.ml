open Feed

type entry = Id | Title | Link | Summary
type feed = Feed_title | Feed_icon | Entry of entry Scrap.t list

let text node = String.concat "" (Soup.texts node)

let scrap_entry ~parse_uri node t = function
  | Id ->
    Feed.{ t with id = Some (text node) }
  | Title ->
    Feed.{ t with title = Some (text node) }
  | Link ->
    let link = Soup.attribute "href" node |> Option.map parse_uri in
    { t with link }
  | Summary ->
    let summary = String.concat "\n" (Soup.trimmed_texts node) in
    { t with summary = Some (Text summary) }

let scrap_feed ~parse_uri node (title, icon, entries) = function
  | Feed_title	-> Some (text node), icon, entries
  | Feed_icon		->
    let feed_icon = Soup.attribute "src" node |> Option.map parse_uri in
    title, feed_icon, entries
  | Entry s	->
    let scrap_entry = Scrap.scrap node (scrap_entry ~parse_uri) in
    let e = List.fold_left scrap_entry Feed.empty_entry s in
    title, icon, e :: entries

let scrap_default_title node =
  match Soup.select_one "title" node with
  | Some node -> Soup.leaf_text node
  | None -> None

let scrap_default_icon ~parse_uri node =
  match Soup.select_one "link[rel~=icon][href]" node with
  | Some node -> Some (parse_uri (Soup.R.attribute "href" node))
  | None -> None

let guess_feed_link ~resolve_uri =
  let r = resolve_uri (Uri.of_string ".") in
  match Uri.scheme r with
  | Some ("http" | "https") -> Some r
  | Some _ | None -> None

let scrap ~resolve_uri node s =
  let parse_uri s = resolve_uri (Uri.of_string s) in
  let feed_title = scrap_default_title node
  and feed_icon = scrap_default_icon ~parse_uri node
  and feed_link = guess_feed_link ~resolve_uri in
  let feed_title, feed_icon, entries =
    Scrap.scrap node (scrap_feed ~parse_uri) (feed_title, feed_icon, []) s in
  Feed.{ feed_title; feed_icon; feed_link;
         entries = Array.of_list entries }

type t = feed Scrap.t

(** Perform scraping *)
let scrap ~resolve_uri t source =
  scrap ~resolve_uri (source |> Soup.(require % child_element % parse)) t
