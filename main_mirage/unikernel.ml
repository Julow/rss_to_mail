open Mirage_types_lwt

module Main
    (Time : TIME)
    (Resolver: Resolver_lwt.S)
    (Conduit: Conduit_mirage.S)
    (Kv_rw: Mirage_kv_lwt.RW)
= struct
  let start _time _res_dns _conduit _kv_rw =
    Lwt.return_unit
end
