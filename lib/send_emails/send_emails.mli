type rw = {
  read :
    unit -> ([ `Data of Cstruct.t | `Eof ], [ `Msg of string ]) result Lwt.t;
  write : Cstruct.t -> (unit, [ `Msg of string ]) result Lwt.t;
}

module Lwt_scheduler : module type of Colombe.Sigs.Make (Lwt)

module type IO = sig
  val sleep_ns : int64 -> unit Lwt.t
  (** Sleep for a given amount of nanoseconds. Undefined behavior if given a
      negative integer. *)

  val establish_tls : hostname:string -> port:int -> rw Lwt.t
  (** Establish a TLS connection. *)
end

val send :
  io:(module IO) ->
  authenticator:X509.Authenticator.t ->
  Feeds_config.t ->
  Rss_to_mail.mail list ->
  Rss_to_mail.mail list Lwt.t
(** [let unsent_mails = send ~certs conf mails] Sends emails to the email
    address specified by [conf.to_]. Emails that couldn't be sent are returned. *)
