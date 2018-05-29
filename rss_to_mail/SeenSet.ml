module SeenSet = Map.Make (String)

(** Store the pair (date, ID) but sort only on the date *)
module RemovedQueue = Heap.Make (struct
	type t = int64 * string
	let leq (a, _) (b, _) = Int64.(<=) a b
end)

type t = {
	seen		: int64 option SeenSet.t;
	removed		: RemovedQueue.t
}

let empty = { seen = SeenSet.empty; removed = RemovedQueue.empty }

let is_seen id t = SeenSet.mem id t.seen

let add id t = { t with seen = SeenSet.add id None t.seen }

(** Add the date to the [removed] queue
	Also set the date in the [seen] set,
	in case the same ID is added or removed again *)
let remove date id t =
	let removed = RemovedQueue.add t.removed (date, id)
	and seen = SeenSet.add id (Some date) t.seen in
	{ seen; removed }

let remove_now id t = { t with seen = SeenSet.remove id t.seen }

(** Take from the [removed] queue
	while the removed date is lower than [since]
	Before removing, re-check the date in the [seen] set *)
let rec filter_removed since t =
	match RemovedQueue.take t.removed with
	| Some (removed, (date, id)) when Int64.(<) date since ->
		let seen =
			match SeenSet.get id t.seen with
			| Some (Some date') when Int64.(<) date' since ->
				SeenSet.remove id t.seen
			| _ -> t.seen
		in
		filter_removed since { seen; removed }
	| _ -> t
