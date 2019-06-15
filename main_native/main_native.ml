(** Ensures [f] is running at most [n] times concurrently
    	Internally uses an Lwt_pool of [unit] *)
let pooled n f =
  let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
  fun x -> Lwt_pool.use pool (fun () -> f x)

module Fetch =
struct

  (** Wrapper around cohttp's [get] that support following redirections
      		If there is chain of more than [max_redirect] redirections,
      		simply returns the last response (with status 30x).
      		Same if there is no "Location" header *)
  let rec get ?(max_redirect=5) url =
    let open Cohttp in
    let%lwt resp, body = Cohttp_lwt_unix.Client.get url in
    match resp.status with
    | (`Moved_permanently
      | `Found
      | `Temporary_redirect)
      when max_redirect > 0 ->
      let max_redirect = max_redirect - 1 in
      let headers = Response.headers resp in
      begin match Header.get headers "location" with
        | Some url		-> get ~max_redirect (Uri.of_string url)
        | None			-> Lwt.return (resp, body)
      end
    | _ -> Lwt.return (resp, body)

  type error = [ `System of string | `Http of int | `Unknown ]

  let fetch url =
    Logs.info (fun fmt -> fmt "Fetching %a" Uri.pp url);
    match%lwt get url with
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
  let fetch = pooled 5 fetch

end

module Log =
struct

  let log_e ~url msg = Logs.warn (fun fmt -> fmt "%s: %s" url msg)
  let log_i ~url msg = Logs.info (fun fmt -> fmt "%s: %s" url msg)

  let log_error url = function
    | `Fetch_error (`Http code) ->
      log_e ~url (sprintf "Http error: %d" code)
    | `Fetch_error (`System msg) ->
      log_e ~url (sprintf "Error: %s" msg)
    | `Fetch_error `Unknown ->
      log_e ~url "Unknown error"
    | `Parsing_error ((line, col), msg) ->
      log_e ~url (sprintf "Parsing error: %d:%d: %s" line col msg)

  let log_updated url ~entries =
    log_i ~url (sprintf "%d new entries" entries)

end

module Feed_datas =
struct

  type t = Rss_to_mail.feed_data StringMap.t

  let get t url = StringMap.find_opt url t
  let set t url data = StringMap.add url data t

end

module Rss_to_mail = Rss_to_mail.Make (Fetch) (Log) (Feed_datas)

let lwt_timeout t r =
  let timeout =
    Lwt.bind (Lwt_unix.sleep t) (fun () -> Lwt.fail_with "timeout")
  in
  Lwt.pick [ r; timeout ]

(** Send a list of mail to [to_]
    	Returns the list of unsent emails *)
let send_mails ~random_seed (conf : Persistent_data.config) mails =
  let send (i, (t : Rss_to_mail.mail)) =
    Logs.debug (fun fmt -> fmt "Sending \"%s\" \"%s\"" t.sender t.subject);
    let server = conf.server in
    let auth =
      let encode s = Base64.encode_string ~pad:true s in
      let `Plain (login, password) = conf.server_auth in
      encode login, encode password
    in
    let from = Some t.sender, conf.from_address in
    let to_ = None, conf.to_address in
    let boundary = "rss_to_mail-boundary-" ^ random_seed in
    let headers = [
      "Content-Type: multipart/alternative; boundary=" ^ boundary;
      "X-Entity-Ref-ID: " ^ random_seed ^ string_of_int i;
    ] in
    let do_send () =
      let part content_type content =
        Client_unix.stream_of_list @@
        ("--" ^ boundary) :: ("Content-Type: " ^ content_type) :: "" ::
        String.split_on_char '\n' content
      in
      let body = Client_unix.stream_concat [
        part "text/plain" t.body_text;
        part "text/html" t.body_html;
        Client_unix.stream_of_list [ "--" ^ boundary ^ "--" ]
      ] in
      Client_unix.send_mail ~auth ~server ~from ~to_ ~headers t.subject body
      |> Lwt.map (fun () -> None)
      |> lwt_timeout 5.
    in
    Lwt.catch do_send (fun _ ->
        Logs.err (fun fmt -> fmt "Failed sending mail \"%s\"" t.subject);
        Lwt.return (Some t))
  in
  (* At most 2 mails sending in parallel *)
  let send_pooled = pooled 2 send in
  mails
  |> Lwt_list.mapi_p (fun i t -> send_pooled (i, t))
  |> Lwt.map (List.filter_map id)

let feed_datas_file = "feed_datas.sexp"

let parse_config_file config_file =
  let err msg = failwith (sprintf "Error: %s: %s" config_file msg) in
  match CCSexp.parse_file config_file with
  | exception Sys_error msg	-> err msg
  | Error msg					-> err msg
  | Ok sexp						-> Persistent_data.load_feeds sexp

let with_feed_datas config_file f =
  let config = parse_config_file config_file in
  let datas =
    match CCSexp.parse_file feed_datas_file with
    | exception Sys_error _ -> Persistent_data.empty_datas
    | Error _ -> Persistent_data.empty_datas
    | Ok sexp -> Persistent_data.load_feed_datas sexp
  in
  let%lwt datas = f config datas in
  CCSexp.to_file feed_datas_file (Persistent_data.save_feed_datas datas);
  Lwt.return_unit

let run (conf : Persistent_data.config) (datas : Persistent_data.feed_datas) =
  Logs.debug (fun fmt -> fmt "%d feeds" (List.length conf.feeds));
  let now = Unix.time () |> Int64.of_float in
  let%lwt feed_datas, mails = Rss_to_mail.check_all ~now datas.feed_datas conf.feeds in
  Logs.app (fun fmt -> fmt "%d new entries" (List.length mails));
  let%lwt unsent_mails =
    let random_seed = Int64.to_string now in
    send_mails ~random_seed conf (datas.unsent_mails @ mails)
  in
  (match unsent_mails with
   | _ :: _ -> Logs.warn (fun fmt ->
        fmt "%d mails could not be sent" (List.length unsent_mails))
   | [] -> ());
  Lwt.return Persistent_data.{ feed_datas; unsent_mails }

let check_config_file f =
  try ignore (parse_config_file f)
  with Failure msg ->
    Printf.eprintf "The configuration file contains some errors:\n  %s\n" msg;
    exit 1

let run check_config config_file () =
  if check_config
  then check_config_file config_file
  else Lwt_main.run (with_feed_datas config_file run)

let () =
  let open Cmdliner in
  let open Arg in
  let check_config =
    let doc = "Check the configuration file for errors and exit" in
    value & flag & info [ "check-config" ] ~doc

  and config =
    let doc = "Configuration file" in
    value & pos 0 string "feeds.sexp" & info [] ~docv:"CONFIG" ~doc

  and verbose =
    let setup_log level =
      Logs.set_level level;
      Logs.set_reporter (Logs_fmt.reporter ())
    in
    Term.(const setup_log $ Logs_cli.level ())
  in

  let term =
    let doc = "Fetch a list of feeds and send a mail for new entries" in
    Term.(const run $ check_config $ config $ verbose),
    Term.info "rss_to_mail" ~doc
  in
  Term.exit @@ Term.eval term
