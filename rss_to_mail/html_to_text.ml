(** Very simple conversion of html to text. Doesn't handle any kind of layout. *)

open Astring

let get_attr attrs name =
  List.find_map
    (function (_, name'), value when name = name' -> Some value | _ -> None)
    attrs

let decode_tag (_, tag) attrs (content, contains_block) =
  match tag with
  | "a" -> (
      match get_attr attrs "href" with
      | Some href when contains_block -> (`P :: `Txt href :: `S :: content, true)
      | Some href -> (content @ [ `S; `Txt "<"; `Txt href; `Txt ">" ], false)
      | None -> (content, contains_block)
    )
  | "li" -> (`NL :: `Txt "-" :: `S :: content, true)
  | "span" | "b" | "i" -> (content, contains_block)
  | "ul" | _ -> (`P :: (content @ [ `P ]), true)

let rec decode_html' acc contains_block = function
  | [] -> (List.rev acc, contains_block)
  | Feed.Html_T text :: tl ->
      let words = String.fields ~empty:false ~is_sep:Char.Ascii.is_white text in
      let acc =
        List.fold_left (fun acc word -> `Txt word :: `S :: acc) acc words
      in
      decode_html' acc contains_block tl
  | Html_E (tag, attrs, content) :: tl ->
      let content, is_block =
        decode_tag tag attrs (decode_html' [] false content)
      in
      decode_html' (List.rev_append content acc) (contains_block || is_block) tl

let decode_html content = decode_html' [] false content

let fpf = Format.fprintf

let sp_gt =
  let order = function `S -> 1 | `NL -> 2 | `P -> 3 in
  fun a b -> order a > order b

let encode_text ppf elms =
  let rec pp_txt s tl =
    fpf ppf "%s" s;
    match tl with [] -> () | `Txt s :: tl -> pp_txt s tl | _ -> pp_sp `S tl
  and pp_sp max = function
    | [] -> ()
    | ((`S | `NL | `P) as sp) :: tl ->
        let max = if sp_gt sp max then sp else max in
        pp_sp max tl
    | `Txt s :: tl ->
        ( match max with
        | `S -> fpf ppf "@ "
        | `NL -> fpf ppf "@\n"
        | `P -> fpf ppf "@\n@\n"
        );
        pp_txt s tl
  in
  let rec skip_sp = function
    | [] -> ()
    | `Txt s :: tl -> pp_txt s tl
    | (`S | `NL | `P) :: tl -> skip_sp tl
  in
  skip_sp elms

let convert content =
  let elms, _ = decode_html content in
  Format.asprintf "@[%a@]" encode_text elms
