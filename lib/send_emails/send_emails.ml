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

module Lwt_scheduler = Colombe.Sigs.Make (Lwt)

type rw = {
  read :
    unit -> ([ `Data of Cstruct.t | `Eof ], [ `Msg of string ]) result Lwt.t;
  write : Cstruct.t -> (unit, [ `Msg of string ]) result Lwt.t;
}

module type IO = sig
  val sleep_ns : int64 -> unit Lwt.t
  val establish_tls : hostname:string -> port:int -> rw Lwt.t
end

(** Cancel a thread after [t] milliseconds raising the exception [Timeout]. *)
let lwt_timeout ~io:(module Io : IO) fail time_ms r =
  let timeout =
    Lwt.bind (Io.sleep_ns Int64.(mul (of_int time_ms) 1_000_000L)) fail
  in
  Lwt.pick [ r; timeout ]

(** Construct a [rw] from a [flow]. This is needed to make colombe communicate
    with a flow. *)
let colombe_rw_of_flow rw =
  let open Lwt.Syntax in
  let rbuf = ref Cstruct.empty in
  let rd_blit bytes off outlen =
    (* [rbuf] is not empty. *)
    let l = min outlen (Cstruct.length !rbuf) in
    Cstruct.blit_to_bytes !rbuf 0 bytes off l;
    rbuf := Cstruct.shift !rbuf l;
    Lwt.return (`Len l)
  in
  let rd () bytes off outlen =
    Lwt_scheduler.inj
    @@
    if Cstruct.length !rbuf = 0
    then
      (* Refill *)
      let* r = rw.read () in
      match r with
      | Ok (`Data cstr) ->
          rbuf := cstr;
          rd_blit bytes off outlen
      | Ok `Eof -> Lwt.return `End
      | Error (`Msg err) -> failwith err
    else rd_blit bytes off outlen
  in
  let wr () str off len =
    Lwt_scheduler.inj
    @@
    let cstruct = Cstruct.of_string ~off ~len str in
    let* r = rw.write cstruct in
    match r with Ok x -> Lwt.return x | Error (`Msg err) -> failwith err
  in
  { Colombe.Sigs.rd; wr }

open Mrmime

let ( >>* ) x f = match x with Ok x -> f x | Error _ as e -> e
let ( let* ) = ( >>* )
let map_error ~f x = match x with Ok _ as ok -> ok | Error e -> Error (f e)

let make_address ?name addr =
  let name = Option.map (fun n -> [ `Word (`String n) ]) name in
  let* local, domain = Emile.address_of_string addr in
  Ok Emile.{ name; local; domain }

let pp_error ppf error =
  let p s = Format.fprintf ppf s in
  match error with
  | `Timeout -> p "Timeout"
  | `Sendmail_error e -> Sendmail.pp_error ppf e
  | `Make_mail_error _ -> p "Internal error"
  | `Uncaught_exception exn -> Fmt.exn ppf exn

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

let sendmail ~io ~hostname ~port ~domain ~authenticator ~authentication sender
    recipients stream =
  let module Io = (val io : IO) in
  let ( <.> ) f g x = f (g x) in
  let lwt_bind x f =
    let open Lwt.Infix in
    let open Lwt_scheduler in
    (* inj (prj x >>= fun x -> f (prj x)) *)
    inj (prj x >>= (prj <.> f))
  in
  let lwt =
    {
      Colombe.Sigs.bind = lwt_bind;
      return = (fun x -> Lwt_scheduler.inj (Lwt.return x));
    }
  in
  let ctx = Colombe.State.Context.make () in
  let hostname = Domain_name.to_string hostname in
  let stream () = Lwt_scheduler.inj (stream ()) in
  Lwt.bind (Io.establish_tls ~hostname ~port) @@ fun rw ->
  Lwt_scheduler.prj
  @@ Sendmail.sendmail lwt (colombe_rw_of_flow rw) () ctx ~domain
       ?authentication sender recipients stream

let send ~io ~authenticator (conf : Feeds_config.t) mails =
  let module Io = (val io : IO) in
  let hostname, port = conf.server in
  let hostname = Domain_name.of_string_exn hostname in
  let domain = Colombe.Domain.of_string_exn "localhost" in
  let (`Plain (username, password)) = conf.server_auth in
  let authentication =
    Some Sendmail.{ username; password; mechanism = PLAIN }
  in
  (* Send one email *)
  let send t =
    match make_mail conf t with
    | Ok (mail, from, recipient) ->
        let stream = lwt_stream (Mt.to_stream mail) in
        Lwt.catch
          (fun () ->
            sendmail ~io ~hostname ~port ~domain ~authenticator ~authentication
              from [ recipient ] stream
            |> Lwt.map (function
                 | Error e -> Error (`Sendmail_error e)
                 | Ok () as ok -> ok
                 )
            |> lwt_timeout ~io (fun () -> Lwt.return_error `Timeout) 15000
          )
          (fun exn -> Lwt.return_error (`Uncaught_exception exn))
    | Error e -> Lwt.return_error (`Make_mail_error e)
  in
  (* At most 2 mails sending in parallel *)
  let send = Utils.pooled 2 send in
  (* Log errors. Returns [None] on success and [Some t] on errors. *)
  let handle_error t = function
    | Error error ->
        Logs.err (fun fmt ->
            fmt "@[<2>Failed sending mail %S:@ %a@]" t.Rss_to_mail.subject
              pp_error error
        );
        Some t
    | Ok () ->
        Logs.debug (fun fmt -> fmt "Sent \"%s\"" t.subject);
        None
  in
  mails
  |> Lwt_list.map_p (fun t -> Lwt.map (handle_error t) (send t))
  |> Lwt.map (fun result ->
         let unsent_mails = List.filter_map Fun.id result in
         Logs.app (fun fmt ->
             let tried = List.length mails
             and failed = List.length unsent_mails in
             fmt "%d/%d emails sent" (tried - failed) tried
         );
         unsent_mails
     )
