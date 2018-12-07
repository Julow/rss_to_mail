open Feed

let link title uri = sprintf "<a href=\"%s\">%s</a>" (Uri.to_string uri) title

let opt_link title = function
	| Some uri	-> link title uri
	| None		-> title

let img url width height styles =
	let styles = String.concat ";" styles in
	sprintf "<img width=\"%d\" height=\"%d\" src=\"%s\" style=\"%s\" />"
		width height (Uri.to_string url) styles

let generate ~sender feed options entry =
	let entry_title =
		match entry.title, entry.link with
		| Some t, link				-> opt_link t link
		| None, (Some l as link)	-> opt_link (Uri.to_string l) link
		| None, None				-> "New entry"

	and header =
		String.concat "" @@
		begin match feed.feed_icon with
			| Some url	->
				[ img url 16 16 [
					"display: inline !important";
					"height: 1em !important";
					"margin: 0 0 -0.1em 0 !important"
				] ]
			| None		-> []
		end @
		[ opt_link sender feed.feed_link ] @
		begin match List.filter_map (function
					| { label = Some _ as c; _ }
					| { term = Some _ as c; _ } -> c
					| _ -> None
				) entry.categories with
			| []	-> []
			| lst	-> [ "(" ^ String.concat ", " lst ^ ")" ]
		end @
		Option.map_or [] (fun d -> [ "on " ^ d ]) entry.date @
		begin match entry.authors with
			| []	-> []
			| auts	->
				let author a = opt_link a.author_name a.author_link in
				[ " by " ^ String.concat ", " (List.map author auts) ]
		end @
		begin match options.Feed_options.label with
			| Some l	-> [ " with label " ^ l ]
			| None		-> []
		end

	and attachments =
		let attachment i t =
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
			"Attachment " ^ string_of_int i ^ ": "
				^ link title t.attach_url ^ info
		in
		List.mapi attachment entry.attachments

	and content =
		match entry.content, entry.summary with
		| Some cont, _
		| None, Some cont	-> cont
		| None, None		-> ""

	in
	String.concat "" [
{|<html lang="en">
	<head>
		<style type="text/css">
a { text-decoration: none; }
.entry_title { margin: 0; }
.entry_title a { border-bottom: 1px dashed black; }
.entry_header { margin-top: 0; }
.content { margin: 20px 0 25px 10px; max-width: 600px; }
.thumbnail { display: block; margin: 0 5px 5px 0; width: 60px; height: 60px; }
		</style>
	</head>
	<body>
		<span style="display: none !important; visibility: hidden; width: 0; height: 0; opacity: 0; color: transparent;">
			|}; Option.get header entry.summary; {|
		</span>
		<table>
			<tr>|}; begin
				match entry.thumbnail with
				| Some thbn	->
					{|<td><img class="thumbnail" width="60" height="60"
						src="|} ^ Uri.to_string thbn ^ {|" /></td>|}
				| None		-> {||}
			end; {|<td>
					<h1 class="entry_title">|}; entry_title; {|</h1>
					<p class="entry_header">|}; header; {|</p>
				</td>
			</tr>
		</table>
		<table>|};
			String.concat "" @@
			List.map (fun a -> {|<tr><td>|} ^ a ^ {|</td></tr>|}) attachments
		; {|</table>
		<p class="content">|}; content; {|</p>
	</body>
</html>|}
	]
