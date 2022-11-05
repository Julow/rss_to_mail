type inline
type block
type _ t = (string -> unit) -> unit

let none _ = ()

(* inline *)

let string s k = k s

let list ?(sep = none) l k =
  match l with
  | [] -> ()
  | e :: l ->
      e k;
      List.iter
        (fun e ->
          sep k;
          e k
        )
        l

let link ?mime_type:_ ?text url k =
  let url = Uri.to_string url in
  match text with
  | Some s ->
      k "[";
      k s;
      k "](";
      k url;
      k ")"
  | None -> k url

let feed_icon _ ~alt:_ _ = ()

(* block *)

let content content k =
  k content.Feed.content_text;
  k "\n\n"

let entry_title title title_link k =
  k "# ";
  k title;
  k "\n\n";
  match title_link with
  | Some l ->
      link l k;
      k "\n\n"
  | None -> ()

let entry_header t k =
  t k;
  k "\n\n"

let thumbnail_table _thumbnail rhs k = rhs k

let attachment_table ts k =
  match ts with
  | [] -> ()
  | ts ->
      k "Attachments:\n\n";
      List.iter
        (fun t ->
          t k;
          k "\n"
        )
        ts;
      k "\n"

let code_block ?language code k =
  k "```";
  Option.iter k language;
  k "\n";
  k code;
  if not (String.ends_with ~suffix:"\n" code) then k "\n";
  k "```";
  k "\n\n"

let body ~sender:_ ?hidden_summary:_ entries =
  let buff = Buffer.create 64 in
  let k s = Buffer.add_string buff s in
  List.iter (fun e -> e k) entries;
  Buffer.contents buff
