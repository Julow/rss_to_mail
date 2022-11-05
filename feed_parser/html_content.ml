open Xml

let map_css_styles f styles =
  let buf = Buffer.create (String.length styles) in
  String.split_on_char ';' styles
  |> List.iter (fun s ->
         match String.index_opt s ':' with
         | None -> Buffer.add_string buf s
         | Some i ->
             let key = String.trim (String.sub s 0 i) in
             let value = String.sub s (i + 1) (String.length s - i - 1) in
             let key, value = f key value in
             Buffer.add_string buf key;
             Buffer.add_char buf ':';
             Buffer.add_string buf value;
             Buffer.add_char buf ';'
     );
  Buffer.contents buf

let filter_attrs ~resolve_uri tag attrs =
  List.filter_map
    (fun ((((_, key) as name), value) as attr) ->
      match (tag, key) with
      (* Remove width and height attributes on images *)
      | "img", ("width" | "height") -> None
      | _, "style" ->
          let map_style s v =
            match s with
            (* Turn width and height styles into max-width and max-height *)
            | "width" -> ("max-width", v)
            | "height" -> ("max-height", v)
            | _ -> (s, v)
          in
          Some (name, map_css_styles map_style value)
      (* Resolve URIs *)
      | "a", "href" | "img", "src" ->
          let value = Uri.to_string (resolve_uri (Uri.of_string value)) in
          Some (name, value)
      | _, _ -> Some attr
    )
    attrs

let element ~filter_attrs ((_, tagname) as tag) attrs childs =
  Feed.Html_E (tag, filter_attrs tagname attrs, childs)

let rec of_xml_nodes ~filter_attrs nodes =
  List.map (of_xml_node ~filter_attrs) nodes

and of_xml_node ~filter_attrs = function
  | Text txt -> Feed.Html_T txt
  | Node ((name, attrs), nodes) ->
      element ~filter_attrs name attrs (of_xml_nodes ~filter_attrs nodes)

let to_feed_content content_html =
  let content_text = Html_to_text.convert content_html in
  { Feed.content_text; content_html }

let markup_to_tree ~filter_attrs =
  Markup.trees
    ~text:(fun s -> Feed.Html_T (String.concat "" s))
    ~element:(element ~filter_attrs)

let of_xml ~resolve_uri nodes =
  let filter_attrs = filter_attrs ~resolve_uri in
  to_feed_content (of_xml_nodes ~filter_attrs nodes)

let of_html_text ~resolve_uri contents =
  let filter_attrs = filter_attrs ~resolve_uri in
  let open Markup in
  contents
  |> string
  |> parse_html
  |> signals
  |> markup_to_tree ~filter_attrs
  |> to_list
  |> to_feed_content

let html_text_to_text contents =
  let filter_attrs _ attrs = attrs in
  let open Markup in
  contents
  |> string
  |> parse_html
  |> signals
  |> markup_to_tree ~filter_attrs
  |> to_list
  |> Html_to_text.convert
