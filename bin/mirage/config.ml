open Mirage

let packages =
  [
    package "cohttp-mirage";
    package "rss_to_mail"
      ~sublibs:[ "feeds_config"; "persistent_data"; "send_emails" ];
    package "mirage-kv";
    package "ca-certs-nss";
    package "oneffs" ~pin:"git+https://git.robur.coop/reynir/oneffs";
    package "git-kv" ~max:"0.0.4";
  ]

let runtime_args = [ runtime_arg ~pos:__POS__ "Unikernel.Args.conf_url" ]

let main =
  main "Unikernel.Main" ~runtime_args ~packages
    (time
    @-> pclock
    @-> resolver
    @-> dns_client
    @-> conduit
    @-> block
    @-> git_client
    @-> job
    )

let stack = generic_stackv4v6 default_network
let resolver = resolver_dns stack
let dns = generic_dns_client stack

(* Git client for fetching the configuration. *)
let git_client =
  let git = mimic_happy_eyeballs stack dns (generic_happy_eyeballs stack dns) in
  let tcp = tcpv4v6_of_stackv4v6 stack in
  git_tcp tcp git

(* Conduit for making HTTP requests. *)
let conduit = conduit_direct ~tls:true stack
let db_block = block_of_file "db"

let () =
  register "rss_to_mail"
    [
      main
      $ default_time
      $ default_posix_clock
      $ resolver
      $ dns
      $ conduit
      $ db_block
      $ git_client;
    ]
