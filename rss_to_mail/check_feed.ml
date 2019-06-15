module Make (Fetch : sig
       type error
       val fetch : Uri.t -> (string, error) result Lwt.t
     end) =
struct

  (* Remove date for IDs that disapeared from the feed: 1 month *)
  let remove_date_from = Int64.add 2678400L

  let prepare_mail ~sender feed options entry =
    let label = options.Feed_desc.label in
    Mail_body.gen_mail ~sender ?label feed [ entry ]

  let new_entries remove_date seen_ids entries =
    let ids, news = Array.fold_right (fun e (ids, news) ->
        match Feed.entry_id e with
        | Some id	->
          let news = if SeenSet.is_seen id seen_ids
            then news else e :: news in
          id :: ids, news
        | None		-> ids, news
      ) entries ([], []) in
    SeenSet.new_ids remove_date ids seen_ids, news

  let filter_entries filters entries =
    let match_any_filter =
      function
      | Feed.{ title = Some title; _ } ->
        let match_ (regexp, expctd) =
          try
            ignore (Str.search_forward regexp title 0);
            expctd
          with Not_found ->
            not expctd
        in
        List.exists match_ filters
      | { title = None; _ }			-> true
    in
    match filters with
    | [] -> entries
    | _ -> Array.filter match_any_filter entries

  let sender_name feed_uri feed options =
    let (|||) opt def =
      match opt with
      | Some "" | None	-> def ()
      | Some v			-> v
    in
    options.Feed_desc.title ||| fun () ->
      feed.Feed.feed_title ||| fun () ->
        Uri.host feed_uri ||| fun () ->
          Uri.to_string feed_uri

  let process ~now feed_uri options seen_ids feed =
    let feed = Feed.resolve_urls feed_uri feed in
    let entries = filter_entries options.Feed_desc.filter feed.entries in
    let seen_ids, entries =
      new_entries (remove_date_from now) seen_ids entries
    in
    feed, seen_ids, entries

  let fetch_feed uri =
    let parse_content contents =
      match Feed_parser.parse (`String (0, contents)) with
      | exception Feed_parser.Error (pos, err) -> Error (`Parsing_error (pos, err))
      | feed -> Ok feed
    in
    Lwt.bind (Fetch.fetch uri) begin Lwt.return % function
        | Error e		-> Error (`Fetch_error e)
        | Ok contents	-> parse_content contents
    end

  let check_data ~now options = function
    | Some (last_update, seen_ids) ->
      let uptodate = Utils.is_uptodate now last_update options in
      false, uptodate, seen_ids
    | None -> true, false, SeenSet.empty

  let check ~now uri options data =
    let first_update, uptodate, seen_ids = check_data ~now options data in
    if uptodate then Lwt.return `Uptodate
    else Lwt.bind (fetch_feed uri) begin function
        | Ok feed ->
          let feed, seen_ids, entries =
            process ~now uri options seen_ids feed
          in
          let mails =
            if first_update then []
            else
              let sender = sender_name uri feed options in
              List.map (prepare_mail ~sender feed options) entries
          in
          Lwt.return (`Ok (seen_ids, mails))
        | Error err ->
          Lwt.return err
      end

end
