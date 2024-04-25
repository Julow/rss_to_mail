open Lwt.Syntax

let or_failure = function Ok x -> x | Error (`Msg msg) -> failwith msg

module Diff = struct
  let compute _ _ = Error (`Msg "Diffs are not implemented")
end

let pooled n f =
  let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
  fun x -> Lwt_pool.use pool (fun () -> f x)

module Main
    (Time : Mirage_time.S)
    (Pclock : Mirage_clock.PCLOCK)
    (Resolver : Resolver_mirage.S)
    (Conduit : Conduit_mirage.S)
    (Kv_rw : Mirage_kv.RW) =
struct
  module HttpClient = Cohttp_mirage.Client.Make (Pclock) (Resolver) (Conduit)
  module NSS = Ca_certs_nss.Make (Pclock)

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

  let start _time _clock res_dns conduit _kv_rw =
    let conf = User_conf.conf in
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
    let now = local_timestamp () in
    let state = Persistent_data.empty in
    let feeds_with_id =
      List.map
        (fun ((desc, _) as f) ->
          let url = Feed_desc.url_of_desc desc in
          (Persistent_data.Feed_id.of_url url, f)
        )
        conf.feeds
    in
    let* data, mails = Rss_to_mail.check_all ~now state.data feeds_with_id in
    let+ unsent_mails = Send_emails.send ~io ~authenticator conf mails in
    ignore unsent_mails
end
