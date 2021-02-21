open Tyxml

type inline = Html_types.p_content_fun

type block = Html_types.div_content

type 'a t = 'a Tyxml_html.elt list

let none = []

let string s = [ Html.txt s ]

let list ?sep l =
  let l =
    match sep with Some sep -> Utils.list_interleave sep l | None -> l
  in
  List.concat l

let link ?mime_type ?text url =
  let s = match text with Some s -> s | None -> Uri.path url in
  let a_type =
    match mime_type with
    | Some t -> [ Html.a_mime_type t ]
    | None -> []
  in
  Html.[ a ~a:(a_href (Uri.to_string url) :: a_type) [ txt s ] ]

let content_div cont =
  [ [%html "<div class=\"content\">" cont "</div>"] ]

let content_of_html =
  let mk_attr ((_, k), v) = Html.Unsafe.string_attrib k v in
  let mk_element tagname attrs childs =
    Html.Unsafe.node tagname ~a:(List.map mk_attr attrs) childs
  in
  let rec to_tyxml = function
    | Feed.Html_E ((_, tagname), attrs, childs) ->
        mk_element tagname attrs (list_to_tyxml childs)
    | Html_T text -> Html.txt text
  and list_to_tyxml nodes = List.map to_tyxml nodes in
  list_to_tyxml

let content content = content_div (content_of_html content.Feed.content_html)

let feed_icon url ~alt =
  [
    [%html
      "<img width=\"16\" height=\"16\" src=" (Uri.to_string url) " alt=" alt
        " style=\"display: inline !important; height: 1em !important; margin: \
         0 0 -0.1em 0 !important\" />"];
  ]

let entry_title title entry_link =
  let link =
    match entry_link with
    | Some url -> link url ~text:title
    | None -> string title
  in
  [ [%html "<h1 class=\"entry_title\">" link "</h1>"] ]

let entry_header t = [ Html.p t ]

let thumbnail_table url t =
  let thumbnail =
    [%html
      "<img class=\"thumbnail\" alt=\"thumbnail\" width=\"60\" height=\"60\" \
       src="
      (Uri.to_string url) " />"]
  in
  [ Html.table [ Html.tr [ Html.td [ thumbnail ]; Html.td t ] ] ]

let attachment_table =
  let attachment ts =
    let ts = (ts : inline t :> Html_types.td_content_fun t) in
    [%html "<tr><td>" ts "</td></tr>"]
  in
  function [] -> [] | ts -> [ Html.table (List.map attachment ts) ]

let body ~sender ?hidden_summary entries =
  let entries =
    match entries with
    | [ e ] -> e
    | entries -> List.map (fun e -> [%html "<div>" e "</div>"]) entries
  in
  let hidden_summary =
    match hidden_summary with
    | Some s ->
        [
          [%html
            "<span \
             style=\"display:none;font-size:1px;color:#333333;line-height:1px;max-height:0px;max-width:0px;opacity:0;overflow:hidden;\">"
            [ Html.txt s ] "</span>"];
        ]
    | None -> []
  in
  Format.asprintf "%a" (Tyxml.Html.pp ~indent:true ())
  @@ [%html
       {|
<html lang="en">
  <head>
    <style>
a { text-decoration: none; }
.entry_title { margin: 0; }
.entry_title a { border-bottom: 1px dashed black; }
.entry_header { margin-top: 0; }
.content { margin: 20px 0 25px 10px; max-width: 600px; }
.content img { max-width: 100%; max-height: auto; }
.thumbnail { display: block; margin: 0 5px 5px 0; width: 60px; height: 60px; }
    </style>
    <title>|}
         (Html.txt sender) {|</title>
  </head>
  <body>|} hidden_summary
         entries {|</body>
</html>|}]
