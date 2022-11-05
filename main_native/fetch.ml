open Lwt.Syntax

let () = Conduit_lwt_unix.(tls_library := OpenSSL)

(** Wrapper around cohttp's [get] that support redirections and abstract errors. *)
let rec get ?(max_redirect = 5) url =
  let open Cohttp in
  let open Cohttp_lwt in
  let* resp, body = Cohttp_lwt_unix.Client.get url in
  match resp.status with
  | `OK -> Lwt.return_ok body
  | `Multiple_choices | `Moved_permanently | `Found | `See_other
  | `Temporary_redirect ->
      let headers = Response.headers resp in
      let* () = Body.drain_body body in
      if max_redirect > 0
      then
        let max_redirect = max_redirect - 1 in
        match Header.get headers "location" with
        | Some url -> get ~max_redirect (Uri.of_string url)
        | None -> Lwt.return_error `Broken_redir
      else Lwt.return_error `Too_many_redir
  | status ->
      let* () = Body.drain_body body in
      Lwt.return_error (`Http (Code.code_of_status status))

type error =
  [ `System of string
  | `Too_many_redir
  | `Broken_redir
  | `Http of int
  | `Unknown
  ]

(** Returns the body as a string and handle errors *)
let fetch url =
  Logs.debug (fun fmt -> fmt "Fetching %a" Uri.pp url);
  let fetch' () =
    let* resp = get url in
    match resp with
    | Ok body ->
        let* body = Cohttp_lwt.Body.to_string body in
        Lwt.return (Ok body)
    | Error _ as e -> Lwt.return e
  and handle_exn = function
    | Failure msg | Unix.Unix_error (_, msg, _) -> Error (`System msg)
    | _ -> Error `Unknown
  in
  Lwt.catch fetch' (Lwt.wrap1 handle_exn)

let error_to_string = function
  | `Http code -> Printf.sprintf "Http error: %d" code
  | `System msg -> Printf.sprintf "Error: %s" msg
  | `Too_many_redir -> "Too many redirections"
  | `Broken_redir -> "Broken redirect"
  | `Unknown -> "Unknown error"
