module type IO = sig
  val now : unit -> Ptime.t
  (** Current time from a non-monotonic clock. *)

  val sleep_ns : int64 -> unit Lwt.t
  (** Sleep for a given amount of nanoseconds. Undefined behavior if given a
      negative integer. *)
end

val send :
  io:(module IO) ->
  certs:X509.Certificate.t list ->
  Feeds_config.t ->
  Rss_to_mail.mail list ->
  Rss_to_mail.mail list Lwt.t
(** [let unsent_mails = send ~certs conf mails] Sends emails to the email
    address specified by [conf.to_]. Emails that couldn't be sent are returned. *)
