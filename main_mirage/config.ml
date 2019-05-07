open Mirage

let main =
  foreign
    "Unikernel.Main"
    ~packages:[ package "cohttp-mirage" ]
    (time @-> resolver @-> conduit @-> kv_rw @-> job)

let () =
  let stack = generic_stackv4 default_network in
  let res_dns = resolver_dns stack in
  let conduit = conduit_direct stack in
  let kv_rw = direct_kv_rw "persistent_storage" in
  register "rss_to_mail" [ main $ default_time $ res_dns $ conduit $ kv_rw ]
