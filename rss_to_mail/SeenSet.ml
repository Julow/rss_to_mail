module StringMap = Map.Make (String)

type t = int64 option StringMap.t

let empty = StringMap.empty

let is_seen id t = StringMap.mem id t

let add id t = StringMap.add id None t

let remove date id t = StringMap.add id (Some date) t

let remove_now id t = StringMap.remove id t

let rec filter_removed since t =
	StringMap.filter (fun _ -> function
		| Some date when Int64.(<) date since -> false
		| _ -> true) t

let new_ids date ids t =
	let remove = Some date in
	let t = StringMap.map (function
		| Some _ as removed	-> removed
		| None				-> remove) t in
	List.fold_left (flip add) t ids

let of_list lst = StringMap.of_list lst
let to_list t = StringMap.to_list t

let of_list_filter since lst =
	let filter_add t = function
		| _, Some date when Int64.(<) date since -> t
		| id, removed -> StringMap.add id removed t
	in
	List.fold_left filter_add StringMap.empty lst
