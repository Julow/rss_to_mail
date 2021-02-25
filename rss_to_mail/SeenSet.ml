module StringMap = Map.Make (String)

type t = int64 option StringMap.t

let empty = StringMap.empty

let is_seen id t = StringMap.mem id t

let add id t = StringMap.add id None t

let remove date id t = StringMap.add id (Some date) t

let remove_now id t = StringMap.remove id t

let filter_removed since t =
  StringMap.filter
    (fun _ -> function
      | Some date when Int64.compare date since < 0 -> false
      | _ -> true
      )
    t

let new_ids date ids t =
  let remove = Some date in
  let t =
    StringMap.map (function Some _ as removed -> removed | None -> remove) t
  in
  List.fold_left (fun t id -> add id t) t ids

let fold = StringMap.fold

let to_list = StringMap.bindings

let of_list =
  List.fold_left
    (fun acc (id, date) -> StringMap.add id date acc)
    StringMap.empty
