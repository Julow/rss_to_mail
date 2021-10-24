open Feed

module Render (Impl : sig
  type inline
  type block
  type 'a t

  val none : 'a t
  val string : string -> inline t
  val list : ?sep:'a t -> 'a t list -> 'a t

  val link : ?mime_type:string -> ?text:string -> Uri.t -> inline t
  (** [mime_type] is not shown. *)

  val content : content -> block t
  val feed_icon : Uri.t -> alt:string -> inline t
  val entry_title : string -> Uri.t option -> block t
  val entry_header : inline t -> block t
  val thumbnail_table : Uri.t -> block t -> block t
  val attachment_table : inline t list -> block t
  val body : sender:string -> ?hidden_summary:string -> block t list -> string
end) =
struct
  open Impl

  let opt_link url text =
    match url with Some url -> link url ~text | None -> string text

  let render_entry ~sender ?label feed entry =
    let entry_title =
      let title = Option.value ~default:"New entry" entry.title in
      entry_title title entry.link
    and info_header =
      let feed_title =
        let feed_icon =
          match feed.feed_icon with
          | Some url -> feed_icon url ~alt:sender
          | None -> none
        in
        Some (list [ string "From "; feed_icon; opt_link feed.feed_link sender ])
      and categories =
        let category = function
          | { label = Some _ as c; term = None }
          | { label = None; term = Some _ as c } ->
              c
          | _ -> None
        in
        match List.filter_map category entry.categories with
        | [] -> None
        | lst -> Some (string ("(" ^ String.concat ", " lst ^ ")"))
      and date =
        match entry.date with
        | Some date -> Some (string ("on " ^ date))
        | None -> None
      and authors =
        let author a = opt_link a.author_link a.author_name in
        match List.map author entry.authors with
        | [] -> None
        | ts ->
            let ts = Utils.list_interleave (string ", ") ts in
            Some (list (string "by " :: ts))
      and label = Option.map (fun l -> string ("with label " ^ l)) label in

      entry_header
      @@ list ~sep:(string " ")
      @@ List.filter_map Fun.id
      @@ [ feed_title; categories; date; authors; label ]
    in

    let full_header =
      let header = list [ entry_title; info_header ] in
      match entry.thumbnail with
      | Some url -> thumbnail_table url header
      | None -> header
    and attachments =
      let attachment index t =
        let index = string (string_of_int (index + 1)) in
        let link = link ?mime_type:t.attach_type t.attach_url in
        let info =
          let size = Option.map Utils.size t.attach_size in
          match (size, t.attach_type) with
          | Some s, Some m -> string (" (" ^ s ^ ", " ^ m ^ ")")
          | Some i, None | None, Some i -> string (" (" ^ i ^ ")")
          | None, None -> none
        in
        list [ string "Attachment "; index; string ": "; link; info ]
      in
      match entry.attachments with
      | [] -> none
      | ts -> attachment_table (List.mapi attachment ts)
    and content_ =
      match List.find_map Fun.id [ entry.content; entry.summary ] with
      | Some c -> content c
      | None -> none
    in
    list [ full_header; attachments; content_ ]

  let render_body ~sender ?label ?hidden_summary feed entries =
    List.map (render_entry ~sender ?label feed) entries
    |> body ~sender ?hidden_summary
end

module HtmlRender = Render (Mail_body_html)
module TextRender = Render (Mail_body_text)

let gen_summary = function
  | [] -> None
  | [ { summary = Some s; _ } ] -> Some s.content_text
  | entries ->
      let titles = List.filter_map (fun e -> e.Feed.title) entries in
      Some (String.concat ", " titles)

let gen_mail ~sender ?label feed entries =
  let hidden_summary = gen_summary entries in
  let body_html =
    HtmlRender.render_body ~sender ?label ?hidden_summary feed entries
  and body_text =
    TextRender.render_body ~sender ?label ?hidden_summary feed entries
  in
  (body_html, body_text)
