(** Entry point for the [rss_to_mail] binary. *)

open Lwt.Syntax

let feed_datas_file = "feed_datas.sexp"

let parse_config_file config_file =
  match Sexplib.Sexp.load_sexps config_file with
  | exception Sexplib.Sexp.Parse_error { err_msg; _ } -> failwith err_msg
  | exception (Failure _ as e) -> raise e
  | sexps -> Feeds_config.parse sexps

let with_feed_datas config_file f =
  let config = parse_config_file config_file in
  let datas =
    match Sexplib.Sexp.load_sexps feed_datas_file with
    | exception Sexplib.Sexp.Parse_error { err_msg; _ } -> failwith err_msg
    | exception (Failure _ as e) -> raise e
    | sexp -> Persistent_data.load sexp
  in
  let* datas = f config datas in
  Sexplib.Sexp.save_sexps_mach feed_datas_file (Persistent_data.save datas);
  Lwt.return_unit

let run_command (`Config, config_file) () =
  Lwt_main.run (with_feed_datas config_file Run_main.run)

let check_config_command (`Config, config_file) () =
  try ignore (parse_config_file config_file)
  with Failure msg ->
    Printf.eprintf "The configuration file contains some errors:\n  %s\n" msg;
    exit 1

let run_scraper_command src () =
  let src = Uri.of_string src in
  match Lwt_main.run (Run_scraper.run src) with
  | Ok () -> ()
  | Error e -> Logs.err (fun fmt -> fmt "%a: %s" Uri.pp src e)

let send_test_email (`Config, config_file) () =
  let conf = parse_config_file config_file in
  let success = Lwt_main.run (Run_main.send_test_email conf) in
  if success then Logs.app (fun fmt -> fmt "Success.") else exit 1

let fetch url () =
  let success = Lwt_main.run (Run_main.fetch url) in
  if not success then exit 1

open Cmdliner

let tagged tag arg = Term.app (Term.const (fun v -> (tag, v))) arg

(* The ca-cert library logs warnings that we can't fix and don't have an
   effect. *)
let disable_library_logs () =
  Logs.Src.list ()
  |> List.iter (fun src ->
         match Logs.Src.name src with
         | "ca-certs" -> Logs.Src.set_level src (Some Error)
         | _ -> ()
     )

let verbose =
  let setup_log level =
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    disable_library_logs ()
  in
  Term.(const setup_log $ Logs_cli.level ())

let config_file =
  let doc = "Configuration file" in
  Arg.(
    tagged `Config
    & value
    & pos 0 string "feeds.sexp"
    & info [] ~docv:"CONFIG" ~doc
  )

let default_term, run_cmd =
  let doc = "Fetch a list of feeds and send a mail for new entries" in
  let term = Term.(const run_command $ config_file $ verbose) in
  (term, Cmd.v (Cmd.info "run" ~doc) term)

let check_config_cmd =
  let doc = "Check the configuration file for errors and exit" in
  Cmd.v
    (Cmd.info "check-config" ~doc)
    Term.(const check_config_command $ config_file $ verbose)

let run_scraper_cmd =
  let source_arg =
    let doc = "Url to an html web page or path to local file." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)
  in
  let doc =
    "Run a scraper against a web page. Useful for degugging. Read the scraper \
     definition from stdin."
  in
  Cmd.v
    (Cmd.info "run-scraper" ~doc)
    Term.(const run_scraper_command $ source_arg $ verbose)

let send_test_email_cmd =
  let doc = "Send a test email and exit." in
  Cmd.v
    (Cmd.info "send-test-email" ~doc)
    Term.(const send_test_email $ config_file $ verbose)

let fetch_cmd =
  let src =
    let doc = "Url to a feed or path to a file." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)
  in
  let doc = "Fetch a feed. Can be used to debug a feed." in
  Cmd.v (Cmd.info "fetch" ~doc) Term.(const fetch $ src $ verbose)

let main_cmd =
  let doc = "Fetches RSS feeds and sends emails." in
  Cmd.group ~default:default_term
    (Cmd.info "rss_to_mail" ~doc)
    [
      run_cmd; check_config_cmd; run_scraper_cmd; send_test_email_cmd; fetch_cmd;
    ]

let () = exit (Cmd.eval main_cmd)
