open Lwt.Syntax

module PooledFetch = struct
  type error = Fetch.error

  (** at most 5 fetch at once *)
  let fetch = Utils.pooled 5 Fetch.fetch

  let pp_error = Fetch.pp_error
end

module Rss_to_mail' = Rss_to_mail
module Rss_to_mail = Rss_to_mail.Make (PooledFetch) (Persistent_data.M) (Diff)

(* Passed to [Send_emails]. *)
let io =
  (module struct
    let now = Ptime_clock.now
    let sleep_ns ns = Lwt_unix.sleep (Int64.to_float ns /. 1_000_000.)
  end : Send_emails.IO
  )

let authenticator () =
  match Ca_certs.authenticator () with
  | Error (`Msg e) -> failwith e
  | Ok auth -> auth

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

let run (conf : Feeds_config.t) ({ data; unsent_mails } : Persistent_data.t) =
  let auth = authenticator () in
  Logs.debug (fun fmt -> fmt "%d feeds" (List.length conf.feeds));
  let now = local_timestamp () in
  Logs.app (fun fmt ->
      let to_retry = List.length unsent_mails in
      if to_retry > 0 then fmt "%d unsent emails to try" to_retry
  );
  let* data, mails = Rss_to_mail.check_all ~now data conf.feeds in
  let+ unsent_mails = Send_emails.send ~io ~auth conf (unsent_mails @ mails) in
  { Persistent_data.data; unsent_mails }

let send_test_email (conf : Feeds_config.t) =
  let auth = authenticator () in
  let mail =
    Rss_to_mail'.
      {
        sender = "rss_to_email";
        to_ = Some conf.to_address;
        subject = "[rss_to_email] Test email";
        body_html = "This is a test email from rss_to_email.";
        body_text = "This is a test email from rss_to_email.";
        timestamp = local_timestamp ();
      }
    
  in

  let+ unsent_mail = Send_emails.send ~io ~auth conf [ mail ] in
  unsent_mail = []

let fetch url =
  let+ feed = Rss_to_mail.fetch_one url in
  match feed with
  | Some feed ->
      Print.print_feed feed;
      true
  | None -> false
