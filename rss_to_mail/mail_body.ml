open Feed

let opt_link title = function
	| Some link	-> "<a href=\"" ^ Uri.to_string link ^ "\">" ^ title ^ "</a>"
	| None		-> title

let img url styles =
	let styles = String.concat ";" styles in
	"<img style=\"" ^ styles ^ "\" src=\"" ^ Uri.to_string url ^ "\" />"

let generate feed options entry =
	let categories =
		let labels = List.map (function
			| { label = Some l; _ }	-> l
			| { term = Some t; _ }	-> t
			| _ -> "") entry.categories in
		match labels with
		| []		-> ""
		| l			-> " (" ^ String.concat ", " l ^ ")"
	and authors =
		let author a = opt_link a.author_name a.author_link in
		match List.map author entry.authors with
		| []		-> ""
		| authors	-> " by " ^ String.concat ", " authors
	and date = Option.map_or "" ((^) "on ") entry.date
	and summary =
		match entry.content, entry.summary with
		| Some cont, _		-> cont
		| None, Some sum	-> "<p>" ^ sum ^ "</p>"
		| None, None		-> ""
	and feed_title =
		let icon = match feed.feed_icon with
			| Some url	-> img url [
				"display: inline !important";
				"height: 1em !important;";
				"margin: 0 0 -0.1em 0 !important;" ]
			| None		-> ""
		and title = match options.Feed_options.title, feed.feed_title with
			| Some xtitle, Some title	-> xtitle ^ " (" ^ title ^ ")"
			| None, Some title
			| Some title, None			-> title
			| None, None				-> ""
		in
		opt_link (icon ^ title) feed.feed_link
	and entry_title =
		let thumb = match entry.thumbnail with
			| Some url	-> img url [
				"display: block !important;";
				"max-width: 25em;" ]
			| None		-> ""
		in
		let p t = "<p>" ^ opt_link (t ^ thumb) entry.link ^ "</p>" in
		match entry.title, entry.link with
		| Some t, _		-> p t
		| None, Some l	-> p (Uri.to_string l)
		| None, None	-> ""
	and label =
		match options.Feed_options.label with
		| Some l	-> " with label " ^ l
		| None		-> ""
	and attachments =
		let attachment t =
			let info =
				match Option.(to_list (map Utils.size t.attach_size))
					@ Option.to_list t.attach_type with
				| []		-> ""
				| i			-> " (" ^ String.concat ", " i ^ ")"
			in
			let title =
				match String.Split.right ~by:"/" (Uri.path t.attach_url) with
				| Some (_, title) when String.contains title '.' -> title
				| _			-> Uri.to_string t.attach_url
			in
			let link = opt_link title (Some t.attach_url) in
			"<p>Attachment: " ^ link ^ info ^ "</p>"
		in
		String.concat "" (List.map attachment entry.attachments)
	in
	String.concat "" [
		"<p>Via "; feed_title; categories; "<br/>";
		date; authors; label; "</p>";
		entry_title;
		attachments;
		summary
	]
