open Script_API

exception Child_not_found of string
exception Attribute_not_found of string

type namespace = XmlService.namespace Js.t
type node = XmlService.element Js.t

let child ?ns tag node =
	let child =
		let tag = Js.string tag in
		match ns with
		| Some ns	-> node##getChild_ns tag ns
		| None		-> node##getChild tag
	in
	Js.Opt.get child (fun () -> raise (Child_not_found tag))

let child_opt map ?ns tag node =
	try Some (map (child ?ns tag node))
	with Child_not_found _ -> None

let children ?ns tag (node : node) =
	let childs =
		let tag = Js.string tag in
		match ns with
		| Some ns	-> node##getChildren_ns tag ns
		| None		-> node##getChildren tag
	in
	Js.to_array childs |> Array.to_list

let tag node = Js.to_string node##getName
let text node = Js.to_string node##getText
let raw_text node = node##getText
let node node = node

let attribute attr node =
	Js.Opt.case (node##getAttribute (Js.string attr))
		(fun () -> raise (Attribute_not_found attr))
		(fun attr -> Js.to_string attr##getValue)

let attribute_opt map attr node =
	try Some (map (attribute attr node))
	with Attribute_not_found _ -> None

let namespace ns = XmlService.t##getNamespace (Js.string ns)
let parse data =
	let doc = XmlService.t##parse data in
	Js.Opt.get doc##getRootElement (fun () -> failwith "Empty document")

let create ?ns tag ?(attr=[]) childs =
	let elem =
		let tag = Js.string tag in
		match ns with
		| Some ns	-> XmlService.t##createElement_ns tag ns
		| None		-> XmlService.t##createElement tag
	in
	let set_attr (k, v) =
		ignore (elem##setAttribute (Js.string k) (Js.string v))
	and add_child child =
		ignore (elem##addContent child) in
	List.iter set_attr attr;
	List.iter add_child childs;
	elem

let create_raw_text ?ns tag ?(attr=[]) text =
	let elem = create ?ns tag ~attr [] in
	ignore (elem##setText text);
	elem

let create_text ?ns tag ?(attr=[]) text =
	create_raw_text ?ns tag ~attr (Js.string text)
