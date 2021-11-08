open Sexplib0.Sexp_conv

type t = {
  previous_entries : (string * V1.seen_set) list;
  next_update : (string * int64) list;
  unsent_mails : V1.mail list;
}
[@@deriving of_sexp, sexp_of]

let of_v1 { V1.feed_datas; unsent_mails } =
  {
    previous_entries = List.map (fun (id, _, s) -> (id, s)) feed_datas;
    next_update = List.map (fun (id, t, _) -> (id, t)) feed_datas;
    unsent_mails;
  }

let load = t_of_sexp
let save = sexp_of_t
