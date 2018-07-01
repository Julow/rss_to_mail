(** Needed at least for FROM
	https://en.wikipedia.org/wiki/MIME#Encoded-Word *)
let mime_encode s =
	let s = B64.encode ~pad:false s in
	let chunk = 75 - 12 (* Magic ! *) in
	let rec encode i =
		let c s = "=?UTF-8?B?" ^ s ^ "?=" in
		let s_len = String.length s in
		if i + chunk < s_len
		then c (String.sub s i chunk) :: encode (i + chunk)
		else [ c (String.sub s i (s_len - i)) ]
	in
	String.concat "\r\n " (encode 0)

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
		[|	"--from"; mime_encode from;
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
