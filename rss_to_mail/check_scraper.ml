module Make (Fetch : sig
       type error
       val fetch : Uri.t -> (string, error) result Lwt.t
     end) =
struct

  module Check_feed = Check_feed.Make (Fetch)

  let update ~first_update ~now uri scraper options seen_ids =
    Lwt.bind (Fetch.fetch uri) begin function
      | Error e		-> Lwt.return (`Fetch_error e)
      | Ok contents	->
        let resolve_uri = Uri.resolve "" uri in
        let feed = Scraper.scrap ~resolve_uri scraper contents in
        let feed, seen_ids, entries =
          Check_feed.process ~now uri options seen_ids feed
        in
        let mails =
          if first_update then []
          else
            let sender = Check_feed.sender_name uri feed options in
            List.map (Check_feed.prepare_mail ~sender feed options) entries
        in
        Lwt.return (`Ok (seen_ids, mails))
    end

  let check ~now uri scraper options data =
    let first_update, uptodate, seen_ids =
      match data with
      | Some (last_update, seen_ids) ->
        let uptodate = Utils.is_uptodate now last_update options in
        false, uptodate, seen_ids
      | None -> true, false, SeenSet.empty
    in
    if uptodate then Lwt.return `Uptodate
    else update ~first_update ~now uri scraper options seen_ids

end
