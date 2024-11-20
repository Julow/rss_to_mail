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
      let headers = Cohttp.Response.headers resp in
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
  | `Unknown_scheme
  ]

(** Returns the body as a string and handle errors *)
let http_fetch url =
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

let file_read path =
  Logs.debug (fun fmt -> fmt "Reading %s" path);
  Lwt.catch
    (fun () ->
      Lwt_io.lines_of_file path
      |> Lwt_stream.to_list
      |> Lwt.map (fun lines -> Ok (String.concat "\n" lines))
    )
    (function
      | Unix.Unix_error (e, _, _) ->
          Lwt.return_error (`System (Unix.error_message e))
      | _ -> Lwt.return_error `Unknown
      )

let fetch uri =
  match Uri.scheme uri with
  | Some "http" | Some "https" -> http_fetch uri
  | Some _ -> Lwt.return_error `Unknown_scheme
  | None -> file_read (Uri.path uri)

let pp_error ppf (error : error) =
  let p fmt = Format.fprintf ppf fmt in
  match error with
  | `Http code -> p "Http error: %d" code
  | `System msg -> p "Error: %s" msg
  | `Too_many_redir -> p "Too many redirections"
  | `Broken_redir -> p "Broken redirect"
  | `Unknown -> p "Unknown error"
  | `Unknown_scheme -> p "Unknown URL scheme"
