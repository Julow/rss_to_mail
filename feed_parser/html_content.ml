(** Parse HTML content as Tyxml div element. Wrapped in a div if the root
    element is not a itself div or if there is more than one root. From a string
    or an xmlm node *)

open Xml
open Tyxml

let make_attribs = List.map (fun ((_, k), v) -> Html.Unsafe.string_attrib k v)

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

(* TODO: filter attrs in [of_xml] ? *)

let element ~resolve_uri (_, tag) attrs childs =
  let attrs = filter_attrs ~resolve_uri tag attrs in
  Html.Unsafe.node tag ~a:(make_attribs attrs) childs

let of_xml ~resolve_uri =
  let rec make_node = function
    | Text txt -> Html.txt txt
    | Node ((name, attrs), nodes) ->
        element ~resolve_uri name attrs (make_nodes nodes)
  and make_nodes nodes = List.map make_node nodes in
  function
  | [ Node (((_, "div"), attrs), nodes) ] ->
      Html.div ~a:(make_attribs attrs) (make_nodes nodes)
  | nodes -> Html.div (make_nodes nodes)

let parse ~resolve_uri contents =
  let open Markup in
  contents |> string |> parse_html |> signals
  |> trees
       ~text:(fun s -> Html.txt (String.concat "" s))
       ~element:(element ~resolve_uri)
  |> to_list |> Html.div
