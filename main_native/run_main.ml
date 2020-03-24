module PooledFetch = struct
  type error = Fetch.error

  (** at most 5 fetch at once *)
  let fetch = Utils.pooled 5 Fetch.fetch
end

module Feed_datas = struct
  module StringMap = Map.Make (String)

  type t = Rss_to_mail.feed_data StringMap.t

  let get t url = StringMap.find_opt url t

  let set t url data = StringMap.add url data t
end

module Rss_to_mail = Rss_to_mail.Make (PooledFetch) (Feed_datas)

let parse_certs certs_file =
  let mapped = Unix_cstruct.of_fd Unix.(openfile certs_file [ O_RDONLY ] 0o0) in
  match X509.Certificate.decode_pem_multiple mapped with
  | Error (`Msg e) -> failwith e
  | Ok certs -> certs

let metrics_updates ~mails logs =
  let updated = ref 0 and errors = ref 0 in
  let log_update = function
    | url, `Updated { Rss_to_mail.entries } ->
        incr updated;
        Logs.info (fun fmt -> fmt "%s: %d new entries" url entries)
    | url, `Parsing_error ((line, col), msg) ->
        incr errors;
        Logs.warn (fun fmt ->
            fmt "%s: Parsing error: %d:%d: %s" url line col msg)
    | url, `Fetch_error err ->
        incr errors;
        Logs.warn (fun fmt -> fmt "%s: %s" url (Fetch.error_to_string err))
    | _, `Uptodate -> ()
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

let run ~certs (conf : Persistent_data.config)
    (datas : Persistent_data.feed_datas) =
  let certs = parse_certs certs in
  Logs.debug (fun fmt -> fmt "%d feeds" (List.length conf.feeds));
  let now = Unix.time () |> Int64.of_float in
  let%lwt feed_datas, mails, logs =
    Rss_to_mail.check_all ~now datas.feed_datas conf.feeds
  in
  metrics_updates ~mails:(List.length mails) logs;
  let to_retry = datas.unsent_mails in
  let%lwt unsent_mails =
    let random_seed = Int64.to_string now in
    Mail.send_mails ~certs ~random_seed conf (to_retry @ mails)
  in
  metrics_mails ~to_retry ~unsent_mails;
  Lwt.return Persistent_data.{ feed_datas; unsent_mails }
