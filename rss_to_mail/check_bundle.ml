module Make (Fetch : sig
  type error

  val fetch : Uri.t -> (string, error) result Lwt.t
end) =
struct
  module Check_feed = Check_feed.Make (Fetch)

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
