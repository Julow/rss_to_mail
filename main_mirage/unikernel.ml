open Lwt.Infix
open Mirage_types_lwt

let pooled n f =
  let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
  fun x -> Lwt_pool.use pool (fun () -> f x)

module Fetch = struct
  let rec get ~ctx ?(max_redirect=5) url =
    Cohttp_mirage.Client.get ~ctx url >>= fun (resp, body) ->
    match resp.status with
    | (`Moved_permanently
      | `Found
      | `Temporary_redirect)
      when max_redirect > 0 ->
      let max_redirect = max_redirect - 1 in
      let headers = Cohttp.Response.headers resp in
      begin match Cohttp.Header.get headers "location" with
        | Some url		-> get ~ctx ~max_redirect (Uri.of_string url)
        | None			-> Lwt.return (resp, body)
      end
    | _ -> Lwt.return (resp, body)

  type error = [ `System of string | `Http of int | `Unknown ]

  let fetch ~ctx url =
    Logs.info (fun fmt -> fmt "Fetching %a" Uri.pp url);
    match%lwt get ~ctx url with
    | exception Failure msg					->
      Lwt.return (Error (`System msg))
    | exception Unix.Unix_error (_, msg, _)	->
      Lwt.return (Error (`System msg))
    | exception _ ->
      Lwt.return (Error (`Unknown))
    | { status = `OK; _ }, body	->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      Lwt.return (Ok body)
    | { status; _ }, _			->
      let code = Cohttp.Code.code_of_status status in
      Lwt.return (Error (`Http code))

  (** at most 5 fetch at once *)
  let fetch ~ctx = pooled 5 (fetch ~ctx)
end

module Main
    (Time : TIME)
    (Resolver: Resolver_lwt.S)
    (Conduit: Conduit_mirage.S)
    (Kv_rw: Mirage_kv_lwt.RW)
= struct
  let start _time _res_dns _conduit _kv_rw =
    Lwt.return_unit
end
