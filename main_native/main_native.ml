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

  type error = [ `System of string | `Http of int ]

  let fetch url =
    match%lwt get url with
    | exception Failure msg					->
      Lwt.return (Error (`System msg))
    | exception Unix.Unix_error (_, msg, _)	->
      Lwt.return (Error (`System msg))
    | { status = `OK; _ }, body	->
      let%lwt body = Cohttp_lwt.Body.to_string body in
      Lwt.return (Ok body)
    | { status; _ }, _			->
      let code = Cohttp.Code.code_of_status status in
      Lwt.return (Error (`Http code))

  (** at most 5 fetch at once *)
  let fetch = pooled 5 fetch

end

module Rss_to_mail = Rss_to_mail.Make (Lwt) (Fetch)

let check_feeds ~now feed_datas feeds =
  let get_feed_datas url = StringMap.find_opt url feed_datas in

  let handle_data acc (url, r) =
    let log_e msg = eprintf "%s: %s\n%!" url msg
    and log_i msg = printf "%s: %s\n%!" url msg in
    match r with
    | `Fetch_error (`Http code)			->
      log_e (sprintf "Http error: %d" code); acc
    | `Fetch_error (`System msg)		->
      log_e (sprintf "Error: %s" msg); acc
    | `Parsing_error ((line, col), msg)	->
      log_e (sprintf "Parsing error: %d:%d: %s" line col msg); acc
    | `Uptodate							-> acc
    | `Updated (seen_ids, new_entries)	->
      log_i (sprintf "%d new entries" new_entries);
      StringMap.add url (now, seen_ids) acc
  in

  let handle (datas, mails) (mails', datas') =
    List.fold_left handle_data datas datas', mails' @ mails
  in
  (** Done in 2 passes to improve concurrency *)
  Lwt_list.map_p (Rss_to_mail.check ~now get_feed_datas) feeds
  |> Lwt.map (List.fold_left handle (feed_datas, []))

(** Send a list of mail to [to_]
    	Returns the list of unsent emails *)
let send_mails ~random_seed (conf : Persistent_data.config) mails =
  let send (i, (t : Rss_to_mail.mail)) =
    let server = conf.server in
    let auth =
      let encode s = Base64.encode_string ~pad:true s in
      let `Plain (login, password) = conf.server_auth in
      encode login, encode password
    in
    let from = Some t.sender, conf.from_address in
    let to_ = None, conf.to_address in
    let headers = [ "X-Entity-Ref-ID: " ^ random_seed ^ string_of_int i ] in
    let do_send () =
      Client.send_mail ~auth ~server ~from ~to_ ~headers t.subject t.body
      |> Lwt.map (fun () -> None)
    in
    Lwt.catch do_send (fun _ ->
        eprintf "Failed sending mail \"%s\"\n" t.subject;
        Lwt.return (Some t))
  in
  (* At most 2 mails sending in parallel *)
  let send_pooled = pooled 2 send in
  mails
  |> Lwt_list.mapi_p (fun i t -> send_pooled (i, t))
  |> Lwt.map (List.filter_map id)

let feed_datas_file = "feed_datas.sexp"

let with_feed_datas config_file f =
  match Persistent_data.load_feeds config_file with
  | exception Failure msg ->
    failwith (sprintf "Error: %s: %s\n" config_file msg)
  | config	->
    let datas = Persistent_data.load_feed_datas feed_datas_file in
    let%lwt datas = f config datas in
    Persistent_data.save_feed_datas feed_datas_file datas;
    Lwt.return_unit

let run (conf : Persistent_data.config) (feed_datas, unsent) =
  let now = Unix.time () |> Int64.of_float in
  let%lwt feed_datas, mails = check_feeds ~now feed_datas conf.feeds in
  printf "%d new entries\n" (List.length mails);
  let%lwt unsent =
    let random_seed = Int64.to_string now in
    send_mails ~random_seed conf (unsent @ mails)
  in
  (match unsent with
   | _ :: _ -> printf "%d mails could not be sent\n" (List.length unsent)
   | [] -> ());
  Lwt.return (feed_datas, unsent)

let check_config_file f =
  try ignore (Persistent_data.load_feeds f)
  with Failure msg ->
    eprintf "The configuration file contains some errors:\n  %s\n" msg;
    exit 1

let run check_config config_file =
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
  in

  let term =
    let doc = "Fetch a list of feeds and send a mail for new entries" in
    Term.(const run $ check_config $ config),
    Term.info "rss_to_mail" ~doc
  in
  Term.exit @@ Term.eval term
