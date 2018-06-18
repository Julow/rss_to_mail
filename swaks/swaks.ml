(** Send a mail using swaks *)
let send_mail ~server ?(tls=true) ?auth ~from ~to_
		?(content="text/html") subject body =
	let auth = match auth with
		| None -> [||]
		| Some (`Plain (user, pass)) ->
			[|	"--auth"; "PLAIN";
				"--auth-user"; user;
				"--auth-password"; pass |]
	in
	let args = Array.concat [
		[|	"swaks";
			"--server"; server |];
		if tls then [| "-tls" |] else [||];
		auth;
		[|	"--from"; from;
			"--to"; to_;
			"--h-Subject"; subject;
			"--h-Content-type"; content;
			"--body"; body |]
	] in
	Lwt_process.exec
		~stdin:`Dev_null ~stdout:`Dev_null ~stderr:`Keep
		("", args)
	|> Lwt.map (function
		| Unix.WEXITED 0	-> `Ok
		| WEXITED r			-> `Failed r
		| WSIGNALED s
		| WSTOPPED s		-> `Crashed s)
