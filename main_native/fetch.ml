let () = Conduit_lwt_unix.(tls_library := OpenSSL)

(** Wrapper around cohttp's [get] that support following redirections If there
    is chain of more than [max_redirect] redirections, simply returns the last
    response (with status 30x). Same if there is no "Location" header *)
let rec get ?(max_redirect = 5) url =
  let open Cohttp in
  let%lwt resp, body = Cohttp_lwt_unix.Client.get url in
  match resp.status with
  | `Multiple_choices | `Moved_permanently | `Found | `See_other
  | `Temporary_redirect
    when max_redirect > 0 -> (
      let max_redirect = max_redirect - 1 in
      let headers = Response.headers resp in
      match Header.get headers "location" with
      | Some url -> get ~max_redirect (Uri.of_string url)
      | None -> Lwt.return (resp, body)
    )
  | _ -> Lwt.return (resp, body)

type error =
  [ `System of string
  | `Http of int
  | `Unknown
  ]

(** Returns the body as a string and handle errors *)
let fetch url =
  Logs.info (fun fmt -> fmt "Fetching %a" Uri.pp url);
  match%lwt get url with
  | exception Failure msg -> Lwt.return (Error (`System msg))
  | exception Unix.Unix_error (_, msg, _) -> Lwt.return (Error (`System msg))
  | exception _ -> Lwt.return (Error `Unknown)
  | { status = `OK; _ }, body ->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      Lwt.return (Ok body)
  | { status; _ }, _ ->
      let code = Cohttp.Code.code_of_status status in
      Lwt.return (Error (`Http code))

let error_to_string = function
  | `Http code -> Printf.sprintf "Http error: %d" code
  | `System msg -> Printf.sprintf "Error: %s" msg
  | `Unknown -> "Unknown error"
