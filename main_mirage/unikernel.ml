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
    let get () =
      get ~ctx url >>= function
      | { Cohttp.Response.status = `OK; _ }, body	->
        Cohttp_lwt.Body.to_string body >>= fun body ->
        Lwt.return (Ok body)
      | { Cohttp.Response.status; _ }, _ ->
        let code = Cohttp.Code.code_of_status status in
        Lwt.return (Error (`Http code))
    in
    Lwt.catch get (function
        | Failure msg ->
          Lwt.return (Error (`System msg))
        | Unix.Unix_error (_, msg, _)	->
          Lwt.return (Error (`System msg))
        | _ ->
          Lwt.return (Error (`Unknown))
      )

  (** at most 5 fetch at once *)
  let fetch ~ctx = pooled 5 (fetch ~ctx)
end

module Log =
struct

  let log_error url = function
    | `Fetch_error (`System msg) ->
      Logs.warn (fun fmt -> fmt "%s: Fetch error: %s" url msg)
    | `Fetch_error (`Http code) ->
      Logs.warn (fun fmt -> fmt "%s: Fetch error: Http status %d" url code)
    | `Fetch_error `Unknown ->
      Logs.warn (fun fmt -> fmt "%s: Unknown fetch error" url)
    | `Parsing_error ((line, col), msg) ->
      Logs.warn (fun fmt -> fmt "%s: Parsing error: %d:%d: %s" url line col msg)

  let log_updated url ~entries =
    Logs.info (fun fmt -> fmt "%s: %d new entries" url entries)

end

module StringMap = Map.Make (String)

module Feed_datas =
struct

  type t = Rss_to_mail.feed_data StringMap.t

  let get t url = StringMap.find_opt url t
  let set t url data = StringMap.add url data t

end

module Main
    (Time : TIME)
    (Resolver: Resolver_lwt.S)
    (Conduit: Conduit_mirage.S)
    (Kv_rw: Mirage_kv_lwt.RW)
= struct

  let feeds = [
    Feed_desc.(Feed "https://www.commitstrip.com/feed/", make_options ())
  ]

  let start _time res_dns conduit _kv_rw =
    let module Fetch = struct
      include Fetch
      let ctx = Cohttp_mirage.Client.ctx res_dns conduit
      let fetch uri = fetch ~ctx uri
    end in
    let module Rss_to_mail = Rss_to_mail.Make (Fetch) (Log) (Feed_datas) in
    Rss_to_mail.check_all ~now:0L StringMap.empty feeds
    |> Lwt.map ignore

end
