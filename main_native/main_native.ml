(** Entry point for the [rss_to_mail] binary. *)

open Lwt.Syntax

let feed_datas_file = "feed_datas.sexp"

let parse_config_file config_file =
  match Sexplib.Sexp.load_sexp config_file with
  | exception Sexplib.Sexp.Parse_error { err_msg; _ } -> failwith err_msg
  | exception (Failure _ as e) -> raise e
  | sexp -> Feeds_config.parse sexp

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

let run_command (`Config, config_file) () (`Certs, certs) =
  Lwt_main.run (with_feed_datas config_file (Run_main.run ~certs))

let check_config_command (`Config, config_file) () =
  try ignore (parse_config_file config_file)
  with Failure msg ->
    Printf.eprintf "The configuration file contains some errors:\n  %s\n" msg;
    exit 1

let run_scraper_command src () =
  match Lwt_main.run (Run_scraper.run src) with
  | Ok () -> ()
  | Error e -> Logs.err (fun fmt -> fmt "%s: %s" src e)

let send_test_email (`Config, config_file) () (`Certs, certs) =
  let conf = parse_config_file config_file in
  let success = Lwt_main.run (Run_main.send_test_email ~certs conf) in
  if success then Logs.app (fun fmt -> fmt "Success.") else exit 1

open Cmdliner

let tagged tag arg = Term.app (Term.const (fun v -> (tag, v))) arg

let verbose =
  let setup_log level =
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ())
  in
  Term.(const setup_log $ Logs_cli.level ())

let certs =
  let doc = "Path to certificate bundle file, in pem format." in
  let env = Arg.env_var "CA_BUNDLE" ~doc in
  Arg.(
    tagged `Certs
    & required
    & opt (some file) None
    & info [ "certs" ] ~env ~docs:"FILE" ~doc
  )

let config_file =
  let doc = "Configuration file" in
  Arg.(
    tagged `Config
    & value
    & pos 0 string "feeds.sexp"
    & info [] ~docv:"CONFIG" ~doc
  )

let run_term =
  let doc = "Fetch a list of feeds and send a mail for new entries" in
  ( Term.(const run_command $ config_file $ verbose $ certs),
    Term.info "run" ~doc
  )

let check_config_term =
  let doc = "Check the configuration file for errors and exit" in
  ( Term.(const check_config_command $ config_file $ verbose),
    Term.info "check-config" ~doc
  )

let run_scraper_term =
  let source_arg =
    let doc = "Url to an html web page or path to local file." in
    Arg.(required & pos 0 (some string) None & info [] ~docv:"SRC" ~doc)
  in
  let doc =
    "Run a scraper against a web page. Useful for degugging. Read the scraper \
     definition from stdin."
  in
  ( Term.(const run_scraper_command $ source_arg $ verbose),
    Term.info "run-scraper" ~doc
  )

let send_test_email =
  let doc = "Send a test email and exit." in
  ( Term.(const send_test_email $ config_file $ verbose $ certs),
    Term.info "send-test-email" ~doc
  )

let () =
  Term.exit
  @@ Term.eval_choice run_term
       [ run_term; check_config_term; run_scraper_term; send_test_email ]
