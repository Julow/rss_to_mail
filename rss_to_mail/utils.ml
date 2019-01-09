type mail = {
	sender		: string;
	subject		: string;
	body		: string
}

let is_uptodate now last_update options =
	let open Int64 in
	match options.Feed_options.refresh with
	| `Every h		-> last_update + of_float (h *. 3600.) >= now
	| `At (h, m)	->
		let today_00 = last_update - last_update mod 86400L in
		let due = today_00 + of_int h * 3600L + of_int m * 60L in
		let due = if last_update >= due then due + 86400L else due in
		due >= now

let rec size s u =
	let to_s () = Int64.to_string s ^ u in
	function
	| _ when Int64.Infix.(<) s 1024L -> to_s ()
	| []				-> to_s ()
	| u' :: tl			-> size (Int64.div s 1024L) u' tl

let size s = size s "b" [ "Kb"; "Mb"; "Gb" ]
