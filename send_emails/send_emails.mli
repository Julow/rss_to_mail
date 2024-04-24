val send :
  certs:X509.Certificate.t list ->
  Feeds_config.t ->
  Rss_to_mail.mail list ->
  Rss_to_mail.mail list Lwt.t
(** [let unsent_mails = send ~certs conf mails] Sends emails to the email
    address specified by [conf.to_]. Emails that couldn't be sent are returned. *)
