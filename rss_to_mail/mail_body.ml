open Feed
open Tyxml

let rec list_interleave elt = function
  | [ _ ] as last	-> last
  | hd :: tl		-> hd :: elt :: list_interleave elt tl
  | []			-> []

module Render (Impl : sig

    type inline
    type block

    type 'a t

    val none : 'a t
    val string : string -> inline t
    val list : ?sep:'a t -> 'a t list -> 'a t

    val link : Uri.t -> string -> inline t
    val raw_content : Feed.content -> block t

    val feed_icon : Uri.t -> alt:string -> inline t
    val entry_title : string -> Uri.t option -> block t
    val entry_header : inline t -> block t
    val thumbnail_table : Uri.t -> block t -> block t
    val attachment_table : (Uri.t * string option * string option) list -> block t

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
        | ts -> Impl.list (Impl.string "by " :: list_interleave (Impl.string ", ") ts)

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
      Option.or_ ~else_:entry.summary entry.content
      |> Option.map_or Impl.none Impl.raw_content
    in
    Impl.list [ full_header; attachments; content ]

end

module HtmlRender = Render (struct

  type inline = Html_types.p_content_fun
  type block = Html_types.div_content

  type 'a t = 'a Tyxml_html.elt list

  let none = []

  let string s = [ Html.txt s ]

  let list ?sep l =
    let l = match sep with Some sep -> list_interleave sep l | None -> l in
    List.concat l

  let link url s =
    Html.[ a ~a:[ a_href (Uri.to_string url) ] [ txt s ] ]

  let raw_content =
    let w cont = [ [%html "<div class=\"content\">"[ cont ]"</div>"] ] in
    function
    | Text txt -> w (Html.txt txt)
    | Html node -> w node

  let feed_icon url ~alt =
    [ [%html "<img width=\"16\" height=\"16\"
      src="(Uri.to_string url)"
      alt="alt"
      style=\"display: inline !important;
        height: 1em !important;
        margin: 0 0 -0.1em 0 !important\" />"] ]

  let entry_title title entry_link =
    let link =
      match entry_link with
      | Some url -> link url title
      | None -> string title
    in
    [ [%html "<h1 class=\"entry_title\">"link"</h1>"] ]

  let entry_header t = [ Html.p t ]

  let thumbnail_table url t =
    let thumbnail = [%html
      "<img class=\"thumbnail\" alt=\"thumbnail\"
          width=\"60\" height=\"60\"
          src="(Uri.to_string url)" />"
    ] in
    [ Html.table [ Html.tr [ Html.td [ thumbnail ]; Html.td t ] ] ]

  let attachment_table =
    let attachment index (url, size, mime) =
      let info =
        match size, mime with
        | Some a, Some b -> string (" (" ^ a ^ ", " ^ b ^ ")")
        | Some a, None | None, Some a -> string (" (" ^ a ^ ")")
        | None, None -> none
      and link = link url (Uri.path url)
      and index = string (string_of_int (index + 1)) in
      [%html "<tr><td>Attachment "index": "link""info"</td></tr>"]
    in
    function
    | [] -> []
    | ts -> [ Html.table (List.mapi attachment ts) ]

end)

let gen_entry = HtmlRender.render_entry

let gen_summary sum =
  [%html "<span style=\"display:none;font-size:1px;color:#333333;
		line-height:1px;max-height:0px;max-width:0px;opacity:0;
		overflow:hidden;\">"[ Html.txt sum ]"</span>"]

let gen_mail ~sender ?hidden_summary entries =
  let entries = match entries with
    | [ e ]		-> e
    | entries	-> List.map (fun e -> [%html "<div>" e "</div>"]) entries
  in
  [%html "
<html lang=\"en\">
	<head>
		<style>
a { text-decoration: none; }
.entry_title { margin: 0; }
.entry_title a { border-bottom: 1px dashed black; }
.entry_header { margin-top: 0; }
.content { margin: 20px 0 25px 10px; max-width: 600px; }
.thumbnail { display: block; margin: 0 5px 5px 0; width: 60px; height: 60px; }
		</style>
		<title>" (Html.txt sender) "</title>
	</head>
	<body>"
      (Option.to_list hidden_summary)
      entries
      "</body>
</html>
"]
