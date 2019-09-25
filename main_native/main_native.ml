(** Ensures [f] is running at most [n] times concurrently
    	Internally uses an Lwt_pool of [unit] *)
let pooled n f =
  let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
  fun x -> Lwt_pool.use pool (fun () -> f x)

module PooledFetch =
struct

  type error = Fetch.error

  (** at most 5 fetch at once *)
  let fetch = pooled 5 Fetch.fetch

end

module Log =
struct

  let log_e ~url msg = Logs.warn (fun fmt -> fmt "%s: %s" url msg)
  let log_i ~url msg = Logs.info (fun fmt -> fmt "%s: %s" url msg)

  let log_error url = function
    | `Fetch_error err ->
      log_e ~url (Fetch.error_to_string err)
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

module Rss_to_mail = Rss_to_mail.Make (PooledFetch) (Log) (Feed_datas)

exception Timeout

let lwt_timeout t r =
  let timeout = Lwt.bind (Lwt_unix.sleep t) (fun () -> Lwt.fail Timeout) in
  Lwt.pick [ r; timeout ]

let stream_concat streams =
  let streams = ref streams in
  let rec next () =
    match !streams with
    | s :: tl ->
        begin match s () with
        | Some _ as r -> r
        | None -> streams := tl; next ()
        end
    | [] -> None
  in
  next

let stream_of_strings lst =
  let ptr = ref lst in
  fun () ->
    match !ptr with
    | hd :: tl ->
      ptr := tl;
      Some (hd, 0, String.length hd)
    | [] -> None

let stream_of_multiline_string s =
  let i = ref 0 in
  fun () ->
    let off = !i in
    if off >= String.length s then None
    else
      let next =
        match String.index_from s off '\n' with
        | exception Not_found -> String.length s
        | next -> next
      in
      i := next + 1;
      Some (s, off, next - off)

(** Yields "\r\n" after every elements in [t] *)
let stream_strings_to_lines t =
  let nl = ref false in
  fun () ->
    if !nl then (
      nl := false;
      Some ("\r\n", 0, 2)
    )
    else match t () with
      | None -> None
      | Some _ as r ->
        nl := true;
        r

let send_mail (conf : Persistent_data.config) body =
  let open Colombe in
  let hostname, port = conf.server in
  let hostname = Domain_name.of_string_exn hostname
  and domain = Domain.of_string_exn hostname in
  let from, _ =
    Reverse_path.Parser.of_string
      (Printf.sprintf "<%s>" conf.from_address)
  in
  let recipients = [
    fst @@
    Forward_path.Parser.of_string
      (Printf.sprintf "<%s>" conf.to_address)
  ] in
  let `Plain (username, password) = conf.server_auth in
  let auth = Some (Auth.make ~username password) in
  let%lwt authenticator = X509_lwt.authenticator `No_authentication_I'M_STUPID in
  Sendmail_lwt.run ~hostname ~port ~domain ~authenticator ~from ~recipients auth body

(** Send a list of mail to [to_]
    	Returns the list of unsent emails *)
let send_mails ~random_seed conf mails =
  let send (i, (t : Rss_to_mail.mail)) =
    Logs.debug (fun fmt -> fmt "Sending \"%s\" \"%s\"" t.sender t.subject);
    let boundary = "rss_to_mail-boundary-" ^ random_seed in
    let headers = [
      Printf.sprintf "From: %s <%s>" t.sender conf.Persistent_data.from_address;
      "Subject: " ^ t.subject;
      "Content-Type: multipart/alternative; boundary=" ^ boundary;
      "X-Entity-Ref-ID: " ^ random_seed ^ string_of_int i;
    ] in
    let part content_type content =
      stream_concat [
        stream_of_strings [
          "--" ^ boundary;
          "Content-Type: " ^ content_type;
          "";
        ];
        stream_of_multiline_string content
      ]
    in
    let body =
      stream_strings_to_lines @@
      stream_concat [
        stream_of_strings headers;
        part "text/plain" t.body_text;
        part "text/html" t.body_html;
        stream_of_strings [ "--" ^ boundary ^ "--"; "" ]
      ]
    in
    send_mail conf body
  in
  (* At most 2 mails sending in parallel *)
  let send_pooled = pooled 2 send in
  mails
  |> Lwt_list.mapi_p (fun i t ->
      match%lwt send_pooled (i, t) |> lwt_timeout 15. with
      | exception Timeout ->
        Logs.err (fun fmt -> fmt "Timed out sending mail \"%s\"" t.subject);
        Lwt.return_some t
      | Error e ->
        Logs.err (fun fmt ->
            fmt "Failed sending mail \"%s\":\n %a"
              t.subject Sendmail_lwt.pp_error e);
        Lwt.return_some t
      | Ok () -> Lwt.return_none
    )
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
  let unsent_count = List.length datas.unsent_mails in
  if unsent_count > 0 then
    Logs.app (fun fmt -> fmt "%d unsent mails to retry" unsent_count);
  let%lwt unsent_mails =
    let random_seed = Int64.to_string now in
    send_mails ~random_seed conf (datas.unsent_mails @ mails)
  in
  (match unsent_mails with
   | _ :: _ -> Logs.warn (fun fmt ->
        fmt "%d mails could not be sent" (List.length unsent_mails))
   | [] -> ());
  Lwt.return Persistent_data.{ feed_datas; unsent_mails }

(* CLI *)

let run_command (`Config, config_file) () =
  Lwt_main.run (with_feed_datas config_file run)

let check_config_command (`Config, config_file) () =
  try ignore (parse_config_file config_file)
  with Failure msg ->
    Printf.eprintf "The configuration file contains some errors:\n  %s\n" msg;
    exit 1

let run_scraper_command src () =
  match Lwt_main.run (Run_scraper.run src) with
  | Ok () -> ()
  | Error e -> Logs.err (fun fmt -> fmt "%s: %s" src e)

open Cmdliner

let tagged tag arg =
  Term.app (Term.const (fun v -> tag, v)) (Arg.value arg)

let verbose =
  let setup_log level =
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Term.(const setup_log $ Logs_cli.level ())

let config_file =
  let doc = "Configuration file" in
  Arg.(tagged `Config & pos 0 string "feeds.sexp" & info [] ~docv:"CONFIG" ~doc)

let run_term =
  let doc = "Fetch a list of feeds and send a mail for new entries" in
  Term.(const run_command $ config_file $ verbose),
  Term.info "run" ~doc

let check_config_term =
  let doc = "Check the configuration file for errors and exit" in
  Term.(const check_config_command $ config_file $ verbose),
  Term.info "check-config" ~doc

let run_scraper_term =
  let source_arg =
    let doc = "Url to an html web page or path to local file." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)
  in
  let doc = "Run a scraper against a web page. Useful for degugging. \
             Read the scraper definition from stdin." in
  Term.(const run_scraper_command $ source_arg $ verbose),
  Term.info "run-scraper" ~doc

let () =
  Term.exit @@ Term.eval_choice run_term [
    run_term;
    check_config_term;
    run_scraper_term;
  ]
