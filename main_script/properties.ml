type t = < > Js.t

let properties = PropertiesService.t##getUserProperties

let load () = properties##getProperties
let save t = properties##setProperties t

let create_empty () = object%js end

let sheet_id = Js.string "SHEET_ID"

let get_sheet_id t = Js.Optdef.to_option (Js.Unsafe.get t sheet_id)
let set_sheet_id t v = Js.Unsafe.set t sheet_id v

let seen_ids url = Js.string ("SEEN_" ^ url)

let get_feed_data t url =
	let f (ids, updt) = Some (SeenSet.of_list ids, updt) in
	Js.Optdef.case (Js.Unsafe.get t (seen_ids url))
		(const None) (f % Json.unsafe_input)

let set_feed_data t url (ids, last_update) =
	let data = SeenSet.to_list ids, last_update in
	Js.Unsafe.set t (seen_ids url) (Json.output data)
