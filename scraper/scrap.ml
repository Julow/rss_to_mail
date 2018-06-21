type 'a t = R of (string * 'a t) list | T of 'a list

let rec scrap node f acc = function
	| R rules			-> scrap_rules node f acc rules
	| T targets			-> List.fold_left (f node) acc targets

and scrap_rules node f acc = function
	| (sel, t) :: tl	->
		let nodes = Soup.select sel node in
		let scrap_node acc node = scrap node f acc t in
		let acc = Soup.fold scrap_node acc nodes in
		scrap_rules node f acc tl
	| []				-> acc
