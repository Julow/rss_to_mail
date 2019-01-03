(**
 * Parse HTML content as Tyxml div element
 * Wrapped in a div if the root element is not a itself div
 * or if there is more than one root
 * From a string or an xmlm node
 *)

open Xml
open Tyxml

let of_xml =
	let make_attribs = List.map (fun ((_, k), v) -> Html.Unsafe.string_attrib k v) in
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

let of_string content =
	let inp = Xmlm.make_input (`String (0, content)) in
	match parse inp with
	| exception Xmlm.Error ((line, col), err) ->
		let msg = Printf.sprintf "%d:%d: %s" line col (Xmlm.error_message err) in
		let open Html in
		div [
			p [ txt "An error occured while parsing the content:";
				br ();
				code [ txt msg ]
			];
			pre [
				txt content
			]
		]
	| content -> of_xml content
