type t = < > Js.t

let properties = PropertiesService.t##getUserProperties

let load () = properties##getProperties

let sheet_id = Js.string "SHEET_ID"

let get_sheet_id t = Js.Optdef.to_option (Js.Unsafe.get t sheet_id)
let set_sheet_id v = properties##setProperty sheet_id v

let seen_ids url = Js.string ("SEEN_" ^ url)

let get_feed_data t url =
	let f (ids, updt) = Some (updt, SeenSet.of_list ids) in
	Js.Optdef.case (Js.Unsafe.get t (seen_ids url))
		(const None) (f % Json.unsafe_input)

let set_feed_data url (last_update, ids) =
	let data = SeenSet.to_list ids, last_update in
	properties##setProperty (seen_ids url) (Json.output data)

let unsent_mails = Js.string "UNSENT_MAILS"

let get_unsent_mails t =
	Js.Optdef.case (Js.Unsafe.get t unsent_mails) (const []) Json.unsafe_input

let set_unsent_mails mails =
	properties##setProperty unsent_mails (Json.output mails)
