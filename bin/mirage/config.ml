open Mirage

let packages = [
  package "cohttp-mirage";
  package "rss_to_mail";
]

let main =
  main "Unikernel.Main" ~packages
    (time @-> resolver @-> conduit @-> kv_rw @-> job)

let () =
  let stack = generic_stackv4v6 default_network in
  let res_dns = resolver_dns stack in
  let conduit = conduit_direct ~tls:true stack in
  let kv_rw = direct_kv_rw "persistent_storage" in
  register "rss_to_mail" [ main $ default_time $ res_dns $ conduit $ kv_rw ]
