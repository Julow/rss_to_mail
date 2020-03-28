let stream_concat streams =
  let streams = ref streams in
  let rec next () =
    match !streams with
    | s :: tl -> (
        match s () with
        | Some _ as r -> r
        | None ->
            streams := tl;
            next ()
      )
    | [] -> None
  in
  next

let stream_of_strings lst =
  let ptr = ref lst in
  fun () ->
    match !ptr with
    | hd :: tl ->
        ptr := tl;
        Some (hd, 0, String.length hd)
    | [] -> None

let stream_of_multiline_string s =
  let i = ref 0 in
  fun () ->
    let off = !i in
    if off >= String.length s
    then None
    else
      let next =
        match String.index_from s off '\n' with
        | exception Not_found -> String.length s
        | next -> next
      in
      i := next + 1;
      Some (s, off, next - off)

(** Yields "\r\n" after every elements in [t] *)
let stream_strings_to_lines t =
  let nl = ref false in
  fun () ->
    if !nl
    then (
      nl := false;
      Some ("\r\n", 0, 2)
    )
    else
      match t () with
      | None -> None
      | Some _ as r ->
          nl := true;
          r

let send_mail ~certs (conf : Persistent_data.config) ~to_address body =
  let open Colombe in
  let hostname, port = conf.server in
  let hostname = Domain_name.of_string_exn hostname
  and domain = Domain.of_string_exn hostname in
  let from, _ =
    Reverse_path.Parser.of_string (Printf.sprintf "<%s>" conf.from_address)
  in
  let recipients =
    [
      fst
      @@ Forward_path.Parser.of_string (Printf.sprintf "<%s>" to_address);
    ]
  in
  let (`Plain (username, password)) = conf.server_auth in
  let auth = Some (Auth.make ~username password) in
  let authenticator =
    let time () = Some (Ptime_clock.now ()) in
    X509.Authenticator.chain_of_trust ~time certs
  in
  Sendmail_lwt.run ~hostname ~port ~domain ~authenticator ~from ~recipients auth
    body

(** Send a list of mail to [to_] Returns the list of unsent emails *)
let send_mails ~certs ~random_seed conf mails =
  let send (i, (t : Rss_to_mail.mail)) =
    let to_address =
      match t.to_ with
      | Some a -> a
      | None -> conf.Persistent_data.to_address
    in
    Logs.debug (fun fmt -> fmt "Sending \"%s\" \"%s\"" t.sender t.subject);
    let boundary = "rss_to_mail-boundary-" ^ random_seed in
    let headers =
      [
        Printf.sprintf "From: %s <%s>" t.sender
          conf.Persistent_data.from_address;
        Printf.sprintf "To: <%s>" to_address;
        "Subject: " ^ t.subject;
        "Content-Type: multipart/alternative; boundary=" ^ boundary;
        "X-Entity-Ref-ID: " ^ random_seed ^ string_of_int i;
      ]
    in
    let part content_type content =
      stream_concat
        [
          stream_of_strings
            [ "--" ^ boundary; "Content-Type: " ^ content_type; "" ];
          stream_of_multiline_string content;
        ]
    in
    let body =
      stream_strings_to_lines
      @@ stream_concat
           [
             stream_of_strings headers;
             part "text/plain" t.body_text;
             part "text/html" t.body_html;
             stream_of_strings [ "--" ^ boundary ^ "--"; "" ];
           ]
    in
    send_mail ~certs conf ~to_address body
  in
  (* At most 2 mails sending in parallel *)
  let send_pooled = Utils.pooled 2 send in
  mails
  |> Lwt_list.mapi_p (fun i t ->
         match%lwt send_pooled (i, t) |> Utils.lwt_timeout 15. with
         | exception Utils.Timeout ->
             Logs.err (fun fmt -> fmt "Timed out sending mail \"%s\"" t.subject);
             Lwt.return_some t
         | Error e ->
             Logs.err (fun fmt ->
                 fmt "Failed sending mail \"%s\":\n %a" t.subject
                   Sendmail_lwt.pp_error e);
             Lwt.return_some t
         | Ok () -> Lwt.return_none)
  |> Lwt.map (List.filter_map Fun.id)
