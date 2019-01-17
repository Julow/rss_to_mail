(**
 * Parse HTML content as Tyxml div element
 * Wrapped in a div if the root element is not a itself div
 * or if there is more than one root
 * From a string or an xmlm node
 *)

open Xml
open Tyxml

let make_attribs = List.map (fun ((_, k), v) -> Html.Unsafe.string_attrib k v)

let of_xml =
	let rec make_node = function
		| Text txt -> Html.txt txt
		| Node (((_, tag), attrs), nodes) ->
				Html.Unsafe.node tag ~a:(make_attribs attrs) (make_nodes nodes)
			and make_nodes nodes = List.map make_node nodes in
	function
	| [ Node (((_, "div"), attrs), nodes) ] ->
			Html.div ~a:(make_attribs attrs) (make_nodes nodes)
	| nodes ->
			Html.div (make_nodes nodes)

let of_string contents =
	let open Markup in
	contents
	|> string
	|> parse_html
	|> signals
	|> trees
		~text:(fun s -> Html.txt (String.concat "" s))
		~element:(fun (_, tag) attrs childs ->
			Html.Unsafe.node tag ~a:(make_attribs attrs) childs)
	|> to_list
	|> Html.div
