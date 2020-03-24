type mail = Mail_body.t = {
  sender : string;
  subject : string;
  body_html : string;
  body_text : string;
}

type feed_data = int64 * SeenSet.t
(** [last_update * seen_ids] *)

module Make (Fetch : sig
  type error

  val fetch : Uri.t -> (string, error) result Lwt.t
end) (Feed_datas : sig
  type t

  val get : t -> string -> feed_data option

  val set : t -> string -> feed_data -> t
end) =
struct
  type update = { entries : int }

  type error =
    [ `Parsing_error of (int * int) * string
    | `Fetch_error of Fetch.error
    ]

  type log = string * [ `Updated of update | error | `Uptodate ]

  module Check_feed = struct
    (* Remove date for IDs that disapeared from the feed: 1 month *)
    let remove_date_from = Int64.add 2678400L

    let prepare_mail ~sender feed options entry =
      let label = options.Feed_desc.label in
      Mail_body.gen_mail ~sender ?label feed [ entry ]

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

    let sender_name feed_uri feed options =
      let ( ||| ) opt def =
        match opt with Some "" | None -> def () | Some v -> v
      in
      options.Feed_desc.title ||| fun () ->
      feed.Feed.feed_title ||| fun () ->
      Uri.host feed_uri ||| fun () -> Uri.to_string feed_uri

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

    let fetch_feed uri =
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

    let check_data ~now options = function
      | Some (last_update, seen_ids) ->
          let uptodate = Utils.is_uptodate now last_update options in
          (false, uptodate, seen_ids)
      | None -> (true, false, SeenSet.empty)

    let check ~now uri options data =
      let first_update, uptodate, seen_ids = check_data ~now options data in
      if uptodate
      then Lwt.return `Uptodate
      else
        Lwt.bind (fetch_feed uri) (function
          | Ok feed ->
              let feed, seen_ids, entries =
                process ~now uri options seen_ids feed
              in
              let mails =
                if first_update
                then []
                else
                  let sender = sender_name uri feed options in
                  List.map (prepare_mail ~sender feed options) entries
              in
              Lwt.return (`Ok (seen_ids, mails))
          | Error err -> Lwt.return err)
  end

  module Check_scraper = struct
    let update ~first_update ~now uri scraper options seen_ids =
      Lwt.bind (Fetch.fetch uri) (function
        | Error e -> Lwt.return (`Fetch_error e)
        | Ok contents ->
            let resolve_uri = Uri.resolve "" uri in
            let feed = Scraper.scrap ~resolve_uri scraper contents in
            let feed, seen_ids, entries =
              Check_feed.process ~now uri options seen_ids feed
            in
            let mails =
              if first_update
              then []
              else
                let sender = Check_feed.sender_name uri feed options in
                List.map (Check_feed.prepare_mail ~sender feed options) entries
            in
            Lwt.return (`Ok (seen_ids, mails)))

    let check ~now uri scraper options data =
      let first_update, uptodate, seen_ids =
        match data with
        | Some (last_update, seen_ids) ->
            let uptodate = Utils.is_uptodate now last_update options in
            (false, uptodate, seen_ids)
        | None -> (true, false, SeenSet.empty)
      in
      if uptodate
      then Lwt.return `Uptodate
      else update ~first_update ~now uri scraper options seen_ids
  end

  module Check_bundle = struct
    let prepare_bundle ~sender feed options = function
      | [] -> []
      | entries ->
          let label = options.Feed_desc.label in
          [ Mail_body.gen_mail ~sender ?label feed entries ]

    let update ~first_update ~now uri options seen_ids =
      Lwt.bind (Check_feed.fetch_feed uri) (function
        | Error e -> Lwt.return e
        | Ok feed ->
            let feed, seen_ids, entries =
              Check_feed.process ~now uri options seen_ids feed
            in
            let mails =
              if first_update
              then []
              else
                let sender = Check_feed.sender_name uri feed options in
                prepare_bundle ~sender feed options entries
            in
            Lwt.return (`Ok (seen_ids, mails)))

    let check ~now uri options data =
      let first_update, uptodate, seen_ids =
        match data with
        | Some (last_update, seen_ids) ->
            let uptodate = Utils.is_uptodate now last_update options in
            (false, uptodate, seen_ids)
        | None -> (true, false, SeenSet.empty)
      in
      if uptodate
      then Lwt.return `Uptodate
      else update ~first_update ~now uri options seen_ids
  end

  type nonrec mail = mail

  type nonrec feed_data = feed_data

  (** * Check a feed for updates * Returns the list of generated mails and
      updated feed datas * Log informations by calling [log] once for each feed *)
  let check_one ~now feed_datas (feed, options) =
    let updated url = function
      | `Ok (seen_ids, mails) ->
          let seen_ids = SeenSet.filter_removed now seen_ids in
          (url, `Updated (mails, seen_ids))
      | (`Uptodate | `Fetch_error _ | `Parsing_error _) as r -> (url, r)
    in
    let get_feed_data = Feed_datas.get feed_datas in
    match feed with
    | Feed_desc.Feed url ->
        let uri, data = (Uri.of_string url, get_feed_data url) in
        let r = Check_feed.check ~now uri options data in
        Lwt.map (updated url) r
    | Scraper (url, scraper) ->
        let uri, data = (Uri.of_string url, get_feed_data url) in
        let r = Check_scraper.check ~now uri scraper options data in
        Lwt.map (updated url) r
    | Bundle url ->
        let uri, data = (Uri.of_string url, get_feed_data url) in
        let r = Check_bundle.check ~now uri options data in
        Lwt.map (updated url) r

  let reduce_updated ~now (acc_datas, acc_mails, logs) = function
    | url, `Updated (mails, seen_ids) ->
        let data = (now, seen_ids) in
        let logs = (url, `Updated { entries = List.length mails }) :: logs in
        (Feed_datas.set acc_datas url data, mails @ acc_mails, logs)
    | (_, (`Fetch_error _ | `Parsing_error _ | `Uptodate)) as log ->
        (acc_datas, acc_mails, log :: logs)

  (** Update a list of feeds in parallel *)
  let check_all ~now feed_datas feeds =
    Lwt_list.map_p (check_one ~now feed_datas) feeds
    |> Lwt.map (fun results ->
           let feed_datas, mails, logs =
             List.fold_left (reduce_updated ~now) (feed_datas, [], []) results
           in
           (feed_datas, mails, List.rev logs))
end
