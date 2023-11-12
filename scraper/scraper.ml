open Feed

type entry =
  | Id
  | Title
  | Link
  | Summary
  | Thumbnail  (** Targets an [<img>] or a [<a>] element *)
  | Attachment of { attach_type : string option }
      (** Targets an element with a [href] attribute *)

type feed =
  | Feed_title
  | Feed_icon
  | Entry of entry Scrap.t list
  | Default of entry

let text node =
  match Soup.trimmed_texts node with
  | [] -> None
  | texts -> Some (String.concat " " texts)

let uri ~parse_uri node =
  let attr a = Soup.attribute a node |> Option.map parse_uri in
  match Soup.name node with
  | "img" -> attr "src"
  | "a" -> attr "href"
  | "link" -> attr "href"
  | _ -> None

let ( ||| ) a b = match a with Some _ -> a | None -> b

let scrap_entry ~parse_uri node t = function
  | Id -> Feed.{ t with id = text node ||| t.id }
  | Title -> Feed.{ t with title = text node ||| t.title }
  | Link -> { t with link = uri ~parse_uri node ||| t.link }
  | Summary ->
      let summary = String.concat "\n" (Soup.trimmed_texts node) in
      { t with summary = Some (Feed.make_text_content summary) }
  | Thumbnail -> { t with thumbnail = uri ~parse_uri node ||| t.thumbnail }
  | Attachment { attach_type } ->
      let attachments =
        match uri ~parse_uri node with
        | Some attach_url ->
            { attach_url; attach_size = None; attach_type } :: t.attachments
        | None -> t.attachments
      in
      { t with attachments }

(* [defaults] accumulates default fields. *)
let scrap_feed ~parse_uri node (title, icon, entries, defaults) = function
  | Feed_title -> (text node ||| title, icon, entries, defaults)
  | Feed_icon -> (title, uri ~parse_uri node ||| icon, entries, defaults)
  | Entry s ->
      let scrap_entry = Scrap.scrap node (scrap_entry ~parse_uri) in
      let e = List.fold_left scrap_entry defaults s in
      (title, icon, e :: entries, defaults)
  | Default ent ->
      (title, icon, entries, scrap_entry ~parse_uri node defaults ent)

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
  let feed_title, feed_icon, entries, _defaults =
    Scrap.scrap node (scrap_feed ~parse_uri)
      (feed_title, feed_icon, [], Feed.empty_entry)
      s
  in
  let metadata = { Feed.feed_title; feed_icon; feed_link } in
  { Feed.metadata; entries = Array.of_list entries }

type t = feed Scrap.t

(** Perform scraping *)
let scrap ~resolve_uri t source =
  scrap ~resolve_uri (Soup.parse source |> Soup.R.child_element) t
