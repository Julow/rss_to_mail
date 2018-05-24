let rec size s u =
	let to_s () = Int64.to_string s ^ u in
	function
	| _ when Int64.Infix.(<) s 1024L -> to_s ()
	| []				-> to_s ()
	| u' :: tl			-> size (Int64.div s 1024L) u' tl

let size s = size s "b" [ "Kb"; "Mb"; "Gb" ]

let date_string date =
	let date = new%js Js.date_fromTimeValue (Int64.to_float date) in
	Js.to_string date##toLocaleString
