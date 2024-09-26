type mail = {
  sender : string;
  to_ : string option;
  subject : string;
  body_html : string;
  body_text : string;
  timestamp : int64;
}

type 'a state_key =
  | Next_update : int64 state_key
  | Previous_entries : SeenSet.t state_key
  | Page_contents : string state_key

module type FETCH = sig
  type error

  val fetch : Uri.t -> (string, error) result Lwt.t
  val pp_error : Format.formatter -> error -> unit
end

module type STATE = sig
  type t
  type id

  val get : t -> id -> 'a state_key -> 'a option
  val set : t -> id -> 'a state_key -> 'a -> t
  val id_of_url : string -> id
  val pp_id : Format.formatter -> id -> unit
end

module type DIFF = sig
  val compute : string -> string -> (string option, [ `Msg of string ]) result
end

module Make (Fetch : FETCH) (State : STATE) (Diff : DIFF) = struct
  type feed = Feed_desc.t
  type update = { entries : int }

  module Process_feed = struct
    (* Remove date for IDs that disapeared from the feed: 1 month *)
    let remove_date_from = Int64.add 2678400L

    let match_regexp regexp str =
      try
        ignore (Str.search_forward regexp str 0);
        true
      with Not_found -> false

    let match_regexp_opt regexp = function
      | Some str -> match_regexp regexp str
      | None -> false

    let match_regexp_content regexp = function
      | Some c -> match_regexp regexp c.Feed.content_text
      | None -> false

    let rec exec_filter entry = function
      | Feed_desc.And fs -> List.for_all (exec_filter entry) fs
      | Or fs -> List.exists (exec_filter entry) fs
      | Not f -> not (exec_filter entry f)
      | Match_title r -> match_regexp_opt r entry.Feed.title
      | Match_content r ->
          match_regexp_content r entry.summary
          || match_regexp_content r entry.content

    let process_content options entry =
      match options.Feed_desc.content with
      | `Keep -> entry
      | `Remove -> { entry with Feed.summary = None; content = None }

    let process ~now _feed_uri options seen_ids feed =
      let match_any_filter entry =
        match options.Feed_desc.filter with
        | Some f -> exec_filter entry f
        | None -> true
      in
      let new_ids, entries =
        Array.fold_right
          (fun entry (ids, news) ->
            match Feed.entry_id entry with
            | Some id when match_any_filter entry ->
                let entry = process_content options entry in
                let news =
                  if SeenSet.is_seen id seen_ids then news else entry :: news
                in
                (id :: ids, news)
            | Some _ | None -> (ids, news)
            (* Ignore entries without an ID *)
          )
          feed.Feed.entries ([], [])
      in
      let seen_ids =
        SeenSet.new_ids (remove_date_from now) new_ids seen_ids
        |> SeenSet.filter_removed now
      in
      (seen_ids, entries)

    let sender_name feed_uri feed_metadata options =
      let ( ||| ) opt def =
        match opt with Some "" | None -> def () | Some v -> v
      in
      options.Feed_desc.title ||| fun () ->
      feed_metadata.Feed.feed_title ||| fun () ->
      Uri.host feed_uri ||| fun () -> Uri.to_string feed_uri
  end

  let prepare_mail ~now ~subject ~sender feed_metadata options content =
    let label, to_ = Feed_desc.(options.label, options.to_) in
    let body_html, body_text =
      Mail_body.gen_mail ~sender ?label feed_metadata content
    in
    { sender; to_; subject; body_html; body_text; timestamp = now }

  let prepare_bundle ~now ~sender feed_metadata options entries =
    let subject =
      Format.sprintf "%d entries from %s" (List.length entries) sender
    in
    prepare_mail ~now ~subject ~sender feed_metadata options (`Multi entries)

  let prepare_mails ~now ~uri feed_metadata options entries =
    let sender = Process_feed.sender_name uri feed_metadata options in
    let prepare entry =
      let subject =
        match entry.Feed.title with
        | Some title -> title
        | None -> "New entry from " ^ sender
      in
      prepare_mail ~now ~subject ~sender feed_metadata options (`Single entry)
    in
    let too_many_entries =
      match options.Feed_desc.max_entries with
      | Some m when List.length entries > m -> true
      | Some _ | None -> false
    in
    if too_many_entries
    then [ prepare_bundle ~now ~sender feed_metadata options entries ]
    else List.map prepare entries

  let prepare_bundle ~now ~uri feed options entries =
    let sender = Process_feed.sender_name uri feed options in
    prepare_bundle ~now ~sender feed options entries

  let prepare_diff ~now ~uri feed options diff =
    let sender = Process_feed.sender_name uri feed options in
    let subject =
      match feed.Feed.feed_title with
      | Some title -> title
      | None -> "Page changed at " ^ sender
    in
    prepare_mail ~now ~subject ~sender feed options (`Diff diff)

  module Check_feed = struct
    let fetch uri =
      let resolve_uri = Uri.resolve "" uri in
      let parse_content contents =
        match Feed_parser.parse ~resolve_uri (`String (0, contents)) with
        | exception Feed_parser.Error (pos, err) ->
            Error (`Parsing_error (pos, err))
        | feed -> Ok feed
      in
      Fetch.fetch uri
      |> Lwt.map (function
           | Error e -> Error (`Fetch_error e)
           | Ok contents -> parse_content contents
           )

    let update ~now uri options feed previous_entries =
      let first_update, seen_ids =
        match previous_entries with
        | None -> (true, SeenSet.empty)
        | Some seen_ids -> (false, seen_ids)
      in
      let seen_ids, entries =
        Process_feed.process ~now uri options seen_ids feed
      in
      let entries =
        (* Don't send anything on first update *)
        if first_update then [] else entries
      in
      (seen_ids, entries)
  end

  module Check_scraper = struct
    let fetch uri scraper =
      Fetch.fetch uri
      |> Lwt.map (function
           | Error e -> Error (`Fetch_error e)
           | Ok contents ->
               let resolve_uri = Uri.resolve "" uri in
               Ok (Scraper.scrap ~resolve_uri scraper contents)
           )
  end

  module Check_diff = struct
    let fetch uri =
      Fetch.fetch uri
      |> Lwt.map (function
           | Error e -> Error (`Fetch_error e)
           | Ok _ as contents -> contents
           )
  end

  type nonrec mail = mail
  type nonrec 'a state_key = 'a state_key

  let fetch_feed ~uri = function
    | `Feed _ -> Check_feed.fetch uri
    | `Scraper (_, s) -> Check_scraper.fetch uri s

  let log_error id = function
    | `Parsing_error ((line, col), msg) ->
        Logs.warn (fun fmt ->
            fmt "%a: Parsing error: %d:%d: %s" State.pp_id id line col msg
        )
    | `Process_error msg ->
        Logs.warn (fun fmt -> fmt "%a: Processing error: %s" State.pp_id id msg)
    | `Fetch_error err ->
        Logs.warn (fun fmt -> fmt "%a: %a" State.pp_id id Fetch.pp_error err)

  let with_state state feed_id key f =
    let x = State.get state feed_id key in
    f x
    |> Lwt.map (function
         | Error _ as e -> ((fun s -> s), e)
         | Ok (x, r) ->
             let update_state state = State.set state feed_id key x in
             (update_state, Ok r)
         )

  let id_of_regular_feed (`Feed feed_url | `Scraper (feed_url, _)) =
    State.id_of_url feed_url

  let check_next_update ~now state feed_id options k =
    let open Lwt.Syntax in
    match State.get state feed_id Next_update with
    | Some next_update when next_update >= now ->
        Lwt.return (Fun.id, Ok `Uptodate)
    | _ ->
        let+ updt, r = k () in
        let updt state =
          let state = updt state in
          (* Set "next_update" even on error to avoid repeatedly calling them *)
          State.set state feed_id Next_update (Utils.next_update now options)
        in
        (updt, r)

  let log_error feed_id k =
    let open Lwt.Syntax in
    let+ x, r = k () in
    match r with
    | Ok ((`Updated _ | `Uptodate) as y) -> (x, y)
    | Error err ->
        log_error feed_id err;
        (x, `Error)

  let ( let@ ) f k = f k

  (** Check a feed for updates. Returns the list of mails and a function for
      updating the state. [now] is used for forgetting entries some time after
      they have been removed from the feed. *)
  let check_feed ~now state (desc, options) =
    let open Lwt_result.Syntax in
    match desc with
    | #Feed_desc.regular_feed as desc ->
        let feed_id = id_of_regular_feed desc in
        let@ () = log_error feed_id in
        let@ () = check_next_update ~now state feed_id options in
        let@ previous_entries = with_state state feed_id Previous_entries in
        let uri = Uri.of_string (Feed_desc.url_of_regular_feed desc) in
        let+ feed = fetch_feed ~uri desc in
        let seen_ids, entries =
          Check_feed.update ~now uri options feed previous_entries
        in
        let mails =
          prepare_mails ~now ~uri feed.Feed.metadata options entries
        in
        Logs.info (fun fmt ->
            fmt "%a: %d new entries" State.pp_id feed_id (List.length mails)
        );
        (seen_ids, `Updated mails)
    | `Bundle desc ->
        let feed_id = id_of_regular_feed desc in
        let@ () = log_error feed_id in
        let@ () = check_next_update ~now state feed_id options in
        let@ previous_entries = with_state state feed_id Previous_entries in
        let uri = Uri.of_string (Feed_desc.url_of_regular_feed desc) in
        let+ feed = fetch_feed ~uri desc in
        let seen_ids, entries =
          Check_feed.update ~now uri options feed previous_entries
        in
        let mails =
          match entries with
          | [] -> []
          | [ _ ] as entries ->
              prepare_mails ~now ~uri feed.Feed.metadata options entries
          | entries ->
              [ prepare_bundle ~now ~uri feed.Feed.metadata options entries ]
        in
        Logs.info (fun fmt ->
            fmt "%a: %d new entries" State.pp_id feed_id (List.length mails)
        );
        (seen_ids, `Updated mails)
    | `Diff url ->
        let feed_id = State.id_of_url url in
        let@ () = log_error feed_id in
        let@ () = check_next_update ~now state feed_id options in
        let@ previous_contents = with_state state feed_id Page_contents in
        let uri = Uri.of_string url in
        let* contents = Check_diff.fetch uri in
        let* mails =
          match previous_contents with
          | Some previous_contents -> (
              match Diff.compute previous_contents contents with
              | Ok (Some diff) ->
                  Lwt.return_ok
                    [ prepare_diff ~now ~uri Feed.empty_metadata options diff ]
              | Ok None -> Lwt.return_ok []
              | Error (`Msg msg) -> Lwt.return_error (`Process_error msg)
            )
          | None -> (* First update, nothing to compare *) Lwt.return_ok []
        in
        Logs.info (fun fmt -> fmt "%s: page changed" url);
        (* Always record the new content, even if there's no diff *)
        Lwt.return_ok (contents, `Updated mails)

  let reduce_results (acc_datas, acc_mails, updated, errors)
      (update_data, result) =
    let acc_datas = update_data acc_datas in
    match result with
    | `Uptodate -> (acc_datas, acc_mails, updated, errors)
    | `Updated mails -> (acc_datas, mails @ acc_mails, updated + 1, errors)
    | `Error -> (acc_datas, acc_mails, updated, errors + 1)

  (** Update a list of feeds in parallel *)
  let check_all ~now state feeds =
    Lwt_list.map_p (check_feed ~now state) feeds
    |> Lwt.map (fun results ->
           let datas, mails, updated, errors =
             List.fold_left reduce_results (state, [], 0, 0) results
           in
           Logs.app (fun fmt ->
               fmt "%d feeds updated, %d errors, %d new entries" updated errors
                 (List.length mails)
           );
           (datas, mails)
       )
end
