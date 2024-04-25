open Mirage

let packages =
  [
    package "cohttp-mirage";
    package "rss_to_mail";
    package "rss_to_mail"
      ~sublibs:[ "feeds_config"; "persistent_data"; "send_emails" ];
    package "mirage-kv";
    package "ca-certs-nss";
  ]

let main =
  main "Unikernel.Main" ~packages
    (time @-> pclock @-> resolver @-> conduit @-> kv_rw @-> job)

let () =
  let stack = generic_stackv4v6 default_network in
  let res_dns = resolver_dns stack in
  let conduit = conduit_direct ~tls:true stack in
  let kv_rw = direct_kv_rw "persistent_storage" in
  register "rss_to_mail"
    [ main $ default_time $ default_posix_clock $ res_dns $ conduit $ kv_rw ]
