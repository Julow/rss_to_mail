let stream_of_multiline_string s =
  let i = ref 0 and nl = ref false in
  fun () ->
    if !nl
    then (
      nl := false;
      Some ("\r\n", 0, 2)
    )
    else
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
        nl := true;
        Some (s, off, next - off)

let lwt_stream t () = Lwt.return (t ())

open Mrmime

let ( >>* ) x f = match x with Ok x -> f x | Error _ as e -> e
let ( let* ) = ( >>* )
let map_error ~f x = match x with Ok _ as ok -> ok | Error e -> Error (f e)
let ( let^ ) = Lwt.bind

let make_address ?name addr =
  let name = Option.map (fun n -> [ `Word (`String n) ]) name in
  let* local, domain = Emile.address_of_string addr in
  Ok Emile.{ name; local; domain }

module C = Content_type

let content_type ?(charset = "UTF-8") ?(parameters = C.Parameters.default) type_
    subtype =
  let parameters = C.Parameters.add "charset" (`String charset) parameters in
  C.make type_ (C.Subtype.iana_exn type_ subtype) parameters

let make_part content_type body =
  let stream = stream_of_multiline_string body in
  let header =
    Header.of_list [ Field.(make Field_name.content_type Content content_type) ]
  in
  Mt.part ~header stream

let make_multipart_alternative ~header parts =
  let multipart_header =
    let t =
      C.make `Multipart
        (C.Subtype.iana_exn `Multipart "alternative")
        C.Parameters.empty
    in
    Header.of_list [ Field.(make Field_name.content_type Content) t ]
  in
  let parts = Mt.multipart ~rng:Mt.rng ~header:multipart_header parts in
  Mt.make header Mt.multi parts

let ptime_of_int64 t = Int64.to_float t |> Ptime.of_float_s |> Option.get

let make_mail (conf : Feeds_config.t) (mail : Rss_to_mail.mail) =
  let* sender =
    make_address ~name:mail.sender conf.from_address
    |> map_error ~f:(fun e -> `Invalid_sender e)
  in
  let* recipient =
    let recipient =
      match mail.to_ with Some r -> r | None -> conf.to_address
    in
    make_address recipient |> map_error ~f:(fun e -> `Invalid_recipient e)
  in
  let* subject =
    match
      let* i, s = Unstrctrd.of_string (mail.subject ^ "\r\n") in
      if i <> String.length mail.subject + 2 then Error `Partial else Ok s
    with
    | Ok _ as s -> s
    | Error _ ->
        Logs.err (fun fmt -> fmt "Error parsing subject %s" mail.subject);
        let* _, s = Unstrctrd.of_string "New entry" in
        Ok s
  in
  let header =
    Header.of_list
      Field.
        [
          make Field_name.from Mailbox sender;
          make Field_name.subject Unstructured (subject :> Unstructured.elt list);
          make (Field_name.v "To") Addresses [ Address.mailbox recipient ];
          make Field_name.date Date
            (Date.of_ptime ~zone:GMT (ptime_of_int64 mail.timestamp));
        ]
      
  in

  let parts =
    [
      make_part (content_type `Text "plain") mail.body_text;
      make_part (content_type `Text "html") mail.body_html;
    ]
  in
  let* from = Colombe_emile.to_reverse_path sender in
  let* recipient = Colombe_emile.to_forward_path recipient in
  Ok (make_multipart_alternative ~header parts, from, recipient)

(** Send a list of mail to [to_] Returns the list of unsent emails *)
let send_mails ~certs (conf : Feeds_config.t) mails =
  let authenticator =
    let time () = Some (Ptime_clock.now ()) in
    X509.Authenticator.chain_of_trust ~time certs
  in
  let hostname, port = conf.server in
  let hostname = Domain_name.of_string_exn hostname in
  let domain = Colombe.Domain.of_string_exn "localhost" in
  let (`Plain (username, password)) = conf.server_auth in
  let authentication = Sendmail.{ username; password; mechanism = PLAIN } in
  let send t =
    match make_mail conf t with
    | Ok (mail, from, recipient) ->
        let stream = lwt_stream (Mt.to_stream mail) in
        Lwt.catch
          (fun () ->
            Sendmail_lwt.sendmail ~hostname ~port ~domain ~authenticator
              ~authentication from [ recipient ] stream
            |> Lwt.map (function Error e -> `Sendmail_error e | Ok () -> `Ok)
            |> Utils.lwt_timeout (fun () -> Lwt.return `Timeout) 15.
          )
          (fun exn -> Lwt.return (`Uncaught_exception exn))
    | Error e -> Lwt.return (`Make_mail_error e)
  in
  (* At most 2 mails sending in parallel *)
  let send_pooled = Utils.pooled 2 send in
  let failed t fmt =
    Logs.err (fun f ->
        f "Failed sending mail \"%s\": %a" t.Rss_to_mail.subject fmt ()
    );
    Some t
  in
  mails
  |> Lwt_list.map_p (fun t ->
         send_pooled t
         |> Lwt.map (function
              | `Timeout -> failed t (Fmt.any "Timeout")
              | `Sendmail_error e -> failed t (Fmt.const Sendmail.pp_error e)
              | `Make_mail_error _ -> failed t (Fmt.any "Internal error")
              | `Uncaught_exception exn -> failed t (Fmt.const Fmt.exn exn)
              | `Ok ->
                  Logs.debug (fun fmt -> fmt "Sent \"%s\"" t.subject);
                  None
              )
     )
  |> Lwt.map (List.filter_map Fun.id)
