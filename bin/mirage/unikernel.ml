open Lwt.Syntax

module Args = struct
  open Cmdliner

  let conf_url =
    let doc =
      "Git URI of the repository containing the configuration. Only \
       unauthenticated Git servers can be accessed."
    in
    Arg.(required & opt (some string) None & info ~doc [ "conf-git" ])
end

module Main
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Resolver_mirage.S)
    (Conduit : Conduit_mirage.S)
    (Db_block : Mirage_block.S)
    (_ : sig end) =
struct
  module Db_fs = OneFFS.Make (Db_block)
  module HttpClient = Cohttp_mirage.Client.Make (Pclock) (Resolver) (Conduit)
  module NSS = Ca_certs_nss.Make (Pclock)
  module Conf_git = Git_kv.Make (Pclock)

  (** {1 Utils} *)

  let or_failure = function Ok x -> x | Error (`Msg msg) -> failwith msg

  module Diff = struct
    let compute _ _ = Error (`Msg "Diffs are not implemented")
  end

  let pooled n f =
    let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
    fun x -> Lwt_pool.use pool (fun () -> f x)

  (* Passed to [Send_emails]. *)
  let io =
    (module struct
      let sleep_ns = Time.sleep_ns
    end : Send_emails.IO
  )

  (** Like unix timestamps but shifted by the local timezone offset *)
  let local_timestamp () =
    let now = Ptime.v (Pclock.now_d_ps ())
    and tz = Pclock.current_tz_offset_s () in
    let offset =
      match tz with
      | Some tz -> Ptime.Span.of_int_s tz
      | None -> Ptime.Span.zero
    in
    let local_now =
      match Ptime.add_span now offset with Some t -> t | None -> assert false
    in
    Ptime.to_float_s local_now |> Int64.of_float

  (* One minute in ps unit. *)
  let minute_ps = 60_000_000_000L

  (** {1 Persistent data} *)

  let load_data db_fs =
    let* result = Db_fs.read db_fs in
    match result with
    | Ok (Some data) ->
        let data = Sexplib.Sexp.of_string_many data in
        Lwt.return (Persistent_data.load data)
    | Ok None -> Lwt.return Persistent_data.empty
    | Error err ->
        (* Something went wrong with the store. *)
        Format.kasprintf failwith "Failed to load persistent data: %a"
          Db_fs.pp_error err

  let save_data db_fs data =
    let data = Persistent_data.save data in
    let buf = Buffer.create 1024 in
    List.iter (Sexplib.Sexp.to_buffer_mach ~buf) data;
    let* result = Db_fs.write db_fs (Buffer.contents buf) in
    match result with
    | Ok () -> Lwt.return_unit
    | Error err -> Format.kasprintf failwith "%a" Db_fs.pp_write_error err

  (** {1 Entry point} *)

  let start _time _clock res_dns conduit db_block conf_git_ctx conf_git_url :
      unit Lwt.t =
    let* db_fs = Db_fs.connect db_block in
    let* conf_git = Git_kv.connect conf_git_ctx conf_git_url in
    let authenticator = or_failure (NSS.authenticator ()) in
    let module Fetch = struct
      let rec get ~ctx ?(max_redirect = 5) url =
        let* resp, body = HttpClient.get ~ctx url in
        match resp.status with
        | (`Moved_permanently | `Found | `Temporary_redirect)
          when max_redirect > 0 -> (
            let max_redirect = max_redirect - 1 in
            let headers = Cohttp.Response.headers resp in
            match Cohttp.Header.get headers "location" with
            | Some url -> get ~ctx ~max_redirect (Uri.of_string url)
            | None -> Lwt.return (resp, body)
          )
        | _ -> Lwt.return (resp, body)

      type error =
        [ `System of string
        | `Http of int
        | `Unknown
        ]

      let pp_error ppf error =
        let p fmt = Format.fprintf ppf fmt in
        match error with
        | `System msg -> p "Internal error: %s" msg
        | `Http code -> p "Http error status: %d" code
        | `Unknown -> p "Unknown error"

      let fetch ~ctx url =
        Logs.info (fun fmt -> fmt "Fetching %a" Uri.pp url);
        let get () =
          let* resp, body = get ~ctx url in
          match resp with
          | { Cohttp.Response.status = `OK; _ } ->
              let+ body = Cohttp_lwt.Body.to_string body in
              Ok body
          | { Cohttp.Response.status; _ } ->
              let code = Cohttp.Code.code_of_status status in
              Lwt.return (Error (`Http code))
        in
        Lwt.catch get (function
          | Failure msg -> Lwt.return (Error (`System msg))
          | Unix.Unix_error (_, msg, _) -> Lwt.return (Error (`System msg))
          | _ -> Lwt.return (Error `Unknown)
          )

      (** at most 5 fetch at once *)
      let fetch ~ctx = pooled 5 (fetch ~ctx)

      let ctx = HttpClient.ctx res_dns conduit
      let fetch uri = fetch ~ctx uri
    end in
    let module Rss_to_mail = Rss_to_mail.Make (Fetch) (Persistent_data.M) (Diff)
    in
    let rec loop () =
      let now = local_timestamp () in
      let* conf = Conf_git.get conf_git (Mirage_kv.Key.v "feeds.sexp") in
      let conf =
        match conf with
        | Ok conf -> Feeds_config.parse (Sexplib.Sexp.of_string conf)
        | Error err -> Format.kasprintf failwith "%a" Conf_git.pp_error err
      in
      let* state = load_data db_fs in
      let feeds_with_id =
        List.map
          (fun ((desc, _) as f) ->
            let url = Feed_desc.url_of_desc desc in
            (Persistent_data.Feed_id.of_url url, f)
          )
          conf.feeds
      in
      let* data, new_mails =
        Rss_to_mail.check_all ~now state.Persistent_data.data feeds_with_id
      in
      let* unsent_mails =
        Send_emails.send ~io ~authenticator conf (state.unsent_mails @ new_mails)
      in
      let* () = save_data db_fs { Persistent_data.data; unsent_mails } in
      let* () = Time.sleep_ns (Int64.mul 10L minute_ps) in
      loop ()
    in
    loop ()
end
