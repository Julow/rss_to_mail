type mail = Mail_body.t = {
  sender    : string;
  subject   : string;
  body_html : string;
  body_text : string;
}

(** [last_update * seen_ids] *)
type feed_data = int64 * SeenSet.t

module Make (Fetch : sig
       type error
       val fetch : Uri.t -> (string, error) result Lwt.t
     end)
    (Feed_datas : sig
       type t
       val get : t -> string -> feed_data option
       val set : t -> string -> feed_data -> t
     end) =
struct

  type update = {
    entries : int;
  }

  type error = [
    | `Parsing_error of (int * int) * string
    | `Fetch_error of Fetch.error
  ]

  type log = string * [ `Updated of update | error | `Uptodate ]

  module Check_feed = Check_feed.Make (Fetch)
  module Check_scraper = Check_scraper.Make (Fetch)
  module Check_bundle = Check_bundle.Make (Fetch)

  type nonrec mail = mail
  type nonrec feed_data = feed_data

  (**
     	 * Check a feed for updates
     	 * Returns the list of generated mails and updated feed datas
     	 * Log informations by calling [log] once for each feed
     	 *)
  let check_one ~now feed_datas (feed, options) =
    let updated url = function
      | `Ok (seen_ids, mails) ->
        let seen_ids = SeenSet.filter_removed now seen_ids in
        url, `Updated (mails, seen_ids)
      | `Uptodate | `Fetch_error _ | `Parsing_error _ as r ->
        url, r
    in
    let get_feed_data = Feed_datas.get feed_datas in
    match feed with
    | Feed_desc.Feed url		->
      let uri, data = Uri.of_string url, get_feed_data url in
      let r = Check_feed.check ~now uri options data in
      Lwt.map (updated url) r
    | Scraper (url, scraper)	->
      let uri, data = Uri.of_string url, get_feed_data url in
      let r = Check_scraper.check ~now uri scraper options data in
      Lwt.map (updated url) r
    | Bundle url				->
      let uri, data = Uri.of_string url, get_feed_data url in
      let r = Check_bundle.check ~now uri options data in
      Lwt.map (updated url) r

  let reduce_updated ~now (acc_datas, acc_mails, logs) = function
    | url, `Updated (mails, seen_ids) ->
      let data = now, seen_ids in
      let logs = (url, `Update { entries = List.length mails }) :: logs in
      Feed_datas.set acc_datas url data, mails @ acc_mails, logs
    | _, (`Fetch_error _ | `Parsing_error _ | `Uptodate) as log ->
      acc_datas, acc_mails, log :: logs

  (** Update a list of feeds in parallel *)
  let check_all ~now feed_datas feeds =
    Lwt_list.map_p (check_one ~now feed_datas) feeds
    |> Lwt.map (fun results ->
        let feed_datas, mails, logs =
          List.fold_left (reduce_updated ~now) (feed_datas, [], []) results
        in
        feed_datas, mails, List.rev logs)

end
