type mail = {
  sender : string;
  to_ : string option;
  subject : string;
  body_html : string;
  body_text : string;
}

type feed_data = int64 * SeenSet.t
(** [next_update * seen_ids] *)

module Make (Fetch : sig
  type error

  val fetch : Uri.t -> (string, error) result Lwt.t
end) (Feed_datas : sig
  type t

  type id

  val get : t -> id -> feed_data option

  val set : t -> id -> feed_data -> t
end) =
struct
  type update = { entries : int }

  type error =
    [ `Parsing_error of (int * int) * string
    | `Fetch_error of Fetch.error
    ]

  type log = Feed_datas.id * [ `Updated of update | error | `Uptodate ]

  module Process_feed = struct
    (* Remove date for IDs that disapeared from the feed: 1 month *)
    let remove_date_from = Int64.add 2678400L

    (** Empty list of filter match everything *)
    let match_any_filter = function
      | [] -> fun _ -> true
      | filters -> (
          function
          | Feed.{ title = Some title; _ } ->
              let match_ (regexp, expctd) =
                try
                  ignore (Str.search_forward regexp title 0);
                  expctd
                with Not_found -> not expctd
              in
              List.exists match_ filters
          | { title = None; _ } -> true
        )

    let process ~now _feed_uri options seen_ids feed =
      let match_any_filter = match_any_filter options.Feed_desc.filter in
      let new_ids, entries =
        Array.fold_right
          (fun entry (ids, news) ->
            match Feed.entry_id entry with
            | Some id when match_any_filter entry ->
                let news =
                  if SeenSet.is_seen id seen_ids then news else entry :: news
                in
                (id :: ids, news)
            | Some _ | None -> (ids, news)
            (* Ignore entries without an ID *))
          feed.Feed.entries ([], [])
      in
      let seen_ids = SeenSet.new_ids (remove_date_from now) new_ids seen_ids in
      (feed, seen_ids, entries)
  end

  let prepare_mail ~subject ~sender feed options entries =
    let label, to_ = Feed_desc.(options.label, options.to_) in
    let body_html, body_text = Mail_body.gen_mail ~sender ?label feed entries in
    { sender; to_; subject; body_html; body_text }

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
           | Ok contents -> parse_content contents)

    let prepare ~sender feed options entries =
      let prepare entry =
        let subject =
          match entry.Feed.title with
          | Some title -> title
          | None -> "New entry from " ^ sender
        in
        prepare_mail ~subject ~sender feed options [ entry ]
      in
      List.map prepare entries
  end

  module Check_scraper = struct
    let fetch uri scraper =
      Fetch.fetch uri
      |> Lwt.map (function
           | Error e -> Error (`Fetch_error e)
           | Ok contents ->
               let resolve_uri = Uri.resolve "" uri in
               Ok (Scraper.scrap ~resolve_uri scraper contents))
  end

  module Check_bundle = struct
    let prepare ~sender feed options entries =
      let prep subject =
        [ prepare_mail ~subject ~sender feed options entries ]
      in
      match entries with
      | [] -> []
      | [ Feed.{ title = Some title; _ } ] -> prep title
      | _ ->
          prep
            (Format.sprintf "%d entries from %s" (List.length entries) sender)
  end

  type nonrec mail = mail

  type nonrec feed_data = feed_data

  let sender_name feed_uri feed options =
    let ( ||| ) opt def =
      match opt with Some "" | None -> def () | Some v -> v
    in
    options.Feed_desc.title ||| fun () ->
    feed.Feed.feed_title ||| fun () ->
    Uri.host feed_uri ||| fun () -> Uri.to_string feed_uri

  (** [Some (first_update, seen_ids)] if the feed need to be updated. *)
  let should_update ~now data =
    match data with
    | Some (next_update, _) when next_update >= now -> None
    | Some (_, seen_ids) -> Some (false, seen_ids)
    | None -> Some (true, SeenSet.empty)

  (** Call [update] to update the feed. *)
  let check_feed ~now ~feed_id url feed_datas options ~fetch ~prepare =
    let uri = Uri.of_string url in
    let process ~first_update ~seen_ids =
      let next_update = Utils.next_update now options in
      let update_data ~seen_ids feed_datas =
        Feed_datas.set feed_datas feed_id (next_update, seen_ids)
      in
      function
      | Error ((`Fetch_error _ | `Parsing_error _) as error) ->
          (* Set "next_update" even on error to avoid repeatedly calling them *)
          (feed_id, update_data ~seen_ids, error)
      | Ok feed ->
          let feed, seen_ids, entries =
            Process_feed.process ~now uri options seen_ids feed
          in
          let sender = sender_name uri feed options in
          let mails =
            (* Don't send anything on first update *)
            if first_update then [] else prepare ~sender feed options entries
          in
          let seen_ids = SeenSet.filter_removed now seen_ids in
          (feed_id, update_data ~seen_ids, `Updated mails)
    in
    let data = Feed_datas.get feed_datas feed_id in
    match should_update ~now data with
    | None -> Lwt.return (feed_id, Fun.id, `Uptodate)
    | Some (first_update, seen_ids) ->
        Lwt.map (process ~first_update ~seen_ids) (fetch uri)

  (** * Check a feed for updates * Returns the list of generated mails and
      updated feed datas * Log informations by calling [log] once for each feed *)
  let check_feed_desc ~now feed_datas (feed_id, (feed, options)) =
    match feed with
    | Feed_desc.Feed url ->
        let fetch = Check_feed.fetch and prepare = Check_feed.prepare in
        check_feed ~now ~feed_id url feed_datas options ~fetch ~prepare
    | Scraper (url, scraper) ->
        let fetch uri = Check_scraper.fetch uri scraper
        and prepare = Check_feed.prepare in
        check_feed ~now ~feed_id url feed_datas options ~fetch ~prepare
    | Bundle (Feed url) ->
        let fetch = Check_feed.fetch in
        let prepare = Check_bundle.prepare in
        check_feed ~now ~feed_id url feed_datas options ~fetch ~prepare
    | Bundle (Scraper (url, scraper)) ->
        let fetch = Fun.flip Check_scraper.fetch scraper
        and prepare = Check_bundle.prepare in
        check_feed ~now ~feed_id url feed_datas options ~fetch ~prepare

  let reduce_updated (acc_datas, acc_mails, logs) (url, update_data, log) =
    let acc_mails, log =
      match log with
      | `Updated mails ->
          (mails @ acc_mails, `Updated { entries = List.length mails })
      | (`Fetch_error _ | `Parsing_error _ | `Uptodate) as log ->
          (acc_mails, log)
    in
    (update_data acc_datas, acc_mails, (url, log) :: logs)

  (** Update a list of feeds in parallel *)
  let check_all ~now feed_datas feeds =
    Lwt_list.map_p (check_feed_desc ~now feed_datas) feeds
    |> Lwt.map (fun results ->
           let feed_datas, mails, logs =
             List.fold_left reduce_updated (feed_datas, [], []) results
           in
           (feed_datas, mails, List.rev logs))
end
