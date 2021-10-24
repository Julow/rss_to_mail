open Sexplib0.Sexp_conv

type seen_set = (string * int64 option) list [@@deriving of_sexp, sexp_of]

type mail = {
  sender : string;
  to_ : string option;
  subject : string;
  body_html : string;
  body_text : string;
}
[@@deriving of_sexp, sexp_of]

type t = {
  feed_datas : (string * int64 * seen_set) list;
  unsent_mails : mail list;
}
[@@deriving of_sexp, sexp_of]

let of_v0 V0.{ feed_datas; unsent_mails } =
  let seen_set_of_v0 = SeenSet.to_list in
  let data_of_v0 (key, (lu, seen_set)) = (key, lu, seen_set_of_v0 seen_set) in
  let mail_of_v0 Rss_to_mail.{ sender; to_; subject; body_html; body_text } =
    { sender; to_; subject; body_html; body_text }
  in
  {
    feed_datas = List.map data_of_v0 (V0.StringMap.bindings feed_datas);
    unsent_mails = List.map mail_of_v0 unsent_mails;
  }

let load = t_of_sexp
let save = sexp_of_t
