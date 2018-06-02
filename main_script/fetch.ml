type 'a req = string * ((string, int) result -> 'a)

let url url f = url, f

let perform fetchs =
	let requests = new%js Js.array_empty in
	List.iter (fun (url, _) ->
		ignore (requests##push (object%js
			val url = Js.string url
			val muteHttpExceptions = Js._true
		end))
	) fetchs;
	let results = UrlFetchApp.t##fetchAll requests in
	List.mapi (fun i (_, f) ->
		let res = Js.Optdef.get (Js.array_get results i)
			(fun () -> assert false) in
		let code = res##getResponseCode in
		if code = 200
		then f (Ok (Js.to_string res##getContentText))
		else f (Error code)
	) fetchs
