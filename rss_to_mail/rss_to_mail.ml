type mail = Mail_body.t = {
  sender		: string;
  subject		: string;
  body		: string
}

(** [last_update * seen_ids] *)
type feed_data = int64 * SeenSet.t

(** [`Parsing_error ((line, column), message)] *)
type parsing_error = [ `Parsing_error of (int * int) * string ]

module Make (Fetch : sig
       type error
       val fetch : Uri.t -> (string, error) result Lwt.t
     end)
    (Log : sig
       (** [log_error feed_url error] *)
       val log_error :
         string -> [ `Fetch_error of Fetch.error | parsing_error ] -> unit
       (** [log_updated feed_url ~entries] *)
       val log_updated : string -> entries:int -> unit
     end)
    (Feed_datas : sig
       type t
       val get : t -> string -> feed_data option
       val set : t -> string -> feed_data -> t
     end) =
struct

  module Check_feed = Check_feed.Make (Fetch)
  module Check_scraper = Check_scraper.Make (Fetch)
  module Check_bundle = Check_bundle.Make (Fetch)

  type nonrec mail = mail
  type nonrec feed_data = feed_data
  type nonrec parsing_error = parsing_error

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

  let reduce_updated ~now (acc_datas, acc_mails) = function
    | url, `Updated (mails, seen_ids) ->
      let data = now, seen_ids in
      Log.log_updated url ~entries:(List.length mails);
      Feed_datas.set acc_datas url data, mails @ acc_mails
    | url, (`Fetch_error _ | `Parsing_error _ as r) ->
      Log.log_error url r;
      acc_datas, acc_mails
    | _, `Uptodate ->
      acc_datas, acc_mails

  (** Update a list of feeds in parallel *)
  let check_all ~now feed_datas feeds =
    Lwt_list.map_p (check_one ~now feed_datas) feeds
    |> Lwt.map (List.fold_left (reduce_updated ~now) (feed_datas, []))

end
