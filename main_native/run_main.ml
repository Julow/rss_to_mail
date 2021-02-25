open Lwt.Syntax

module PooledFetch = struct
  type error = Fetch.error

  (** at most 5 fetch at once *)
  let fetch = Utils.pooled 5 Fetch.fetch
end

module Rss_to_mail' = Rss_to_mail
module Rss_to_mail = Rss_to_mail.Make (PooledFetch) (Persistent_data.M)

let parse_certs certs_file =
  let mapped = Unix_cstruct.of_fd Unix.(openfile certs_file [ O_RDONLY ] 0o0) in
  match X509.Certificate.decode_pem_multiple mapped with
  | Error (`Msg e) -> failwith e
  | Ok certs -> certs

let metrics_updates ~mails logs =
  let updated = ref 0 and errors = ref 0 in
  let log_update (id, log) =
    let url = Persistent_data.Feed_id.to_string id in
    match log with
    | `Updated { Rss_to_mail.entries } ->
        incr updated;
        Logs.info (fun fmt -> fmt "%s: %d new entries" url entries)
    | `Parsing_error ((line, col), msg) ->
        incr errors;
        Logs.warn (fun fmt ->
            fmt "%s: Parsing error: %d:%d: %s" url line col msg)
    | `Fetch_error err ->
        incr errors;
        Logs.warn (fun fmt -> fmt "%s: %s" url (Fetch.error_to_string err))
    | `Uptodate -> ()
  in
  List.iter log_update logs;
  Logs.app (fun fmt ->
      fmt "%d feeds updated, %d errors, %d new entries" !updated !errors mails)

let metrics_mails ~to_retry ~unsent_mails =
  Logs.app (fun fmt ->
      let retried = List.length to_retry in
      if retried > 0 then fmt "%d unsent emails to retry" retried);
  Logs.warn (fun fmt ->
      let unsent = List.length unsent_mails in
      if unsent > 0 then fmt "%d emails could not be sent" unsent)

(** Like unix timestamps but shifted by the local timezone offset *)
let local_timestamp () =
  let now = Ptime_clock.now () and tz = Ptime_clock.current_tz_offset_s () in
  let offset =
    match tz with Some tz -> Ptime.Span.of_int_s tz | None -> Ptime.Span.zero
  in
  let local_now =
    match Ptime.add_span now offset with Some t -> t | None -> assert false
  in
  Ptime.to_float_s local_now |> Int64.of_float

let run ~certs (conf : Feeds_config.t) (datas : Persistent_data.t) =
  let certs = parse_certs certs in
  Logs.debug (fun fmt -> fmt "%d feeds" (List.length conf.feeds));
  let now = local_timestamp () in
  let feeds_with_id =
    List.map
      (fun ((desc, _) as f) ->
        (Persistent_data.Feed_id.of_url (Feed_desc.url_of_feed desc), f))
      conf.feeds
  in
  let* feed_datas, mails, logs =
    Rss_to_mail.check_all ~now datas.feed_datas feeds_with_id
  in
  metrics_updates ~mails:(List.length mails) logs;
  let to_retry = datas.unsent_mails in
  let+ unsent_mails =
    Mail.send_mails ~certs conf (to_retry @ mails)
  in
  metrics_mails ~to_retry ~unsent_mails;
  { Persistent_data.feed_datas; unsent_mails }

let send_test_email ~certs (conf : Feeds_config.t) =
  let certs = parse_certs certs in
  let mail = Rss_to_mail'.{
    sender = "rss_to_email";
    to_ = Some conf.to_address;
    subject = "[rss_to_email] Test email";
    body_html = "This is a test email from rss_to_email.";
    body_text = "This is a test email from rss_to_email.";
  } in
  let+ unsent_mail = Mail.send_mails ~certs conf [mail] in
  (unsent_mail = [])
