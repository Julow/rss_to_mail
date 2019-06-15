open Feed

module Render (Impl : sig

    type inline
    type block

    type 'a t

    val none : 'a t
    val string : string -> inline t
    val list : ?sep:'a t -> 'a t list -> 'a t

    val link : Uri.t -> string -> inline t

    val raw_content_html : [< Html_types.div ] Tyxml_html.elt -> block t
    val raw_content_text : string -> block t

    val feed_icon : Uri.t -> alt:string -> inline t
    val entry_title : string -> Uri.t option -> block t
    val entry_header : inline t -> block t
    val thumbnail_table : Uri.t -> block t -> block t
    val attachment_table : (Uri.t * string option * string option) list -> block t

    val body : sender:string -> ?hidden_summary:string -> block t list -> string

  end) =
struct

  let opt_link url text =
    match url with
    | Some url -> Impl.link url text
    | None -> Impl.string text

  let render_entry ~sender ?label feed entry =
    let entry_title =
      let title = Option.get "New entry" entry.title in
      Impl.entry_title title entry.link

    and info_header =
      let feed_icon =
        match feed.feed_icon with
        | Some url -> Impl.feed_icon url ~alt:sender
        | None -> Impl.none

      and feed_title = opt_link feed.feed_link sender

      and categories =
        let category = function
          | { label = Some _ as c; term = None }
          | { label = None; term = Some _ as c } -> c
          | _ -> None
        in
        match List.filter_map category entry.categories with
        | []	-> Impl.none
        | lst	-> Impl.string ("(" ^ String.concat ", " lst ^ ")")

      and date =
        match entry.date with
        | Some date -> Impl.string ("on " ^ date)
        | None -> Impl.none

      and authors =
        let author a = opt_link a.author_link a.author_name in
        match List.map author entry.authors with
        | [] -> Impl.none
        | ts ->
          let ts = Utils.list_interleave (Impl.string ", ") ts in
          Impl.list (Impl.string "by " :: ts)

      and label =
        Option.map_or Impl.none (fun l -> Impl.string ("with label " ^ l)) label

      in
      Impl.entry_header @@
      Impl.list ~sep:(Impl.string " ")
        [ feed_icon; feed_title; categories; date; authors; label ]

    in
    let full_header =
      let header = Impl.list [ entry_title; info_header ] in
      match entry.thumbnail with
      | Some url -> Impl.thumbnail_table url header
      | None -> header

    and attachments =
      let attachment t =
        t.attach_url, Option.map Utils.size t.attach_size, t.attach_type
      in
      match entry.attachments with
      | [] -> Impl.none
      | ts -> Impl.attachment_table (List.map attachment ts)

    and content =
      match Option.or_ ~else_:entry.summary entry.content with
      | Some (Html html) -> Impl.raw_content_html html
      | Some (Text txt) -> Impl.raw_content_text txt
      | None -> Impl.none
    in
    Impl.list [ full_header; attachments; content ]

  let render_body ~sender ?label ?hidden_summary feed entries =
    List.map (render_entry ~sender ?label feed) entries
    |> Impl.body ~sender ?hidden_summary

end

module HtmlRender = Render (Mail_body_html)

type t = {
  sender		: string;
  subject		: string;
  body  		: string
}

let gen_summary =
  function
  | [] -> None
  | [ { summary = Some (Feed.Text sum); _ } ] -> Some sum
  | [ { summary = (Some (Feed.Html _) | None); _ } ] -> None
  | entries ->
    let titles = List.filter_map (fun e -> e.Feed.title) entries in
    Some (String.concat ", " titles)

let gen_mail ~sender ?label feed entries =
  let subject =
    match entries with
    | [ { title = Some title; _ } ] -> title
    | [ { title = None; _ } ] -> "New entry from " ^ sender
    | entries ->
      string_of_int (List.length entries) ^ " entries from " ^ sender
  in
  let hidden_summary = gen_summary entries in
  let body =
    HtmlRender.render_body ~sender ?label ?hidden_summary feed entries
  in
  { sender; subject; body }
