module StringMap = Map.Make (String)

type t = V0.t = {
  feed_datas : (int64 * SeenSet.t) StringMap.t;
  unsent_mails : Rss_to_mail.mail list;
}

let empty = V0.empty

let load = function
  | [] -> empty
  | [ sexp ] -> V0.load sexp
  | _ -> failwith "Invalid format"

let save t = [ V0.save t ]
