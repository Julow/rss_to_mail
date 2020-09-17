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
             Buffer.add_char buf ';');
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
      | _, _ -> Some attr)
    attrs

let element ~resolve_uri (_, tagname as tag) attrs childs =
  let attrs = filter_attrs ~resolve_uri tagname attrs in
  Feed.Html_E (tag, attrs, childs)

let rec of_xml_nodes ~resolve_uri nodes =
  List.map (of_xml_node ~resolve_uri) nodes

and of_xml_node ~resolve_uri = function
  | Text txt -> Feed.Html_T txt
  | Node ((name, attrs), nodes) ->
      element ~resolve_uri name attrs (of_xml_nodes ~resolve_uri nodes)

let to_feed_content = function
  | [ Feed.Html_T txt ] -> Feed.Text txt
  | html -> Feed.Html html

let of_xml ~resolve_uri nodes =
  to_feed_content (of_xml_nodes ~resolve_uri nodes)

let parse ~resolve_uri contents =
  let open Markup in
  contents |> string |> parse_html |> signals
  |> trees
       ~text:(fun s -> Feed.Html_T (String.concat "" s))
       ~element:(element ~resolve_uri)
  |> to_list
  |> to_feed_content
