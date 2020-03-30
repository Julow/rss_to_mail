module Load_chain = struct
  type (_, _) chain =
    | ( :: ) :
        (int * ('prev -> 'a) * ('inp -> 'a)) * ('inp, 'prev) chain
        -> ('inp, 'a) chain
    | V0 : int * ('inp -> 'a) -> ('inp, 'a) chain

  (** Load using the given [version] and promote the the lastest type. Raise
      [Failure _] if [version] doesn't match any parser. *)
  let rec load : 'a. version:int -> 'inp -> ('inp, 'a) chain -> 'a =
   fun ~version inp -> function
    | (v', _, load) :: _ when v' = version -> load inp
    | (_, of_prev, _) :: prev -> of_prev (load ~version inp prev)
    | V0 (v', load) when v' = version -> load inp
    | V0 _ -> raise Not_found

  let lastest version of_prev ~load ~save prev =
    (version, save, (version, of_prev, load) :: prev)
end

module StringMap = Map.Make (String)

type t = {
  feed_datas : (int64 * SeenSet.t) StringMap.t;
  unsent_mails : Rss_to_mail.mail list;
}

let empty = { feed_datas = StringMap.empty; unsent_mails = [] }

let last_version, save, versions =
  Load_chain.(V1.(lastest 1 of_v0 ~load ~save) V0.(V0 (0, load)))

let of_v1 V1.{ feed_datas; unsent_mails } =
  let mail_of_v1 V1.{ sender; to_; subject; body_html; body_text } =
    Rss_to_mail.{ sender; to_; subject; body_html; body_text }
  in
  let add_data acc (key, date, seen_set) =
    StringMap.add key (date, SeenSet.of_list seen_set) acc
  in
  {
    feed_datas = List.fold_left add_data StringMap.empty feed_datas;
    unsent_mails = List.map mail_of_v1 unsent_mails;
  }

let to_v1 { feed_datas; unsent_mails } =
  let data_of_v1 (key, (date, seen_set)) =
    (key, date, SeenSet.to_list seen_set)
  in
  let mail_of_v1 Rss_to_mail.{ sender; to_; subject; body_html; body_text } =
    V1.{ sender; to_; subject; body_html; body_text }
  in
  V1.
    {
      feed_datas = List.map data_of_v1 (StringMap.bindings feed_datas);
      unsent_mails = List.map mail_of_v1 unsent_mails;
    }

let load =
  let load_v version sexp = of_v1 (Load_chain.load ~version sexp versions) in
  function
  | [] -> empty
  | [ sexp ] -> load_v 0 sexp
  | Sexplib0.Sexp.[ List [ Atom "version"; Atom v ]; sexp ] ->
      load_v (int_of_string v) sexp
  | _ -> failwith "Invalid format"

let save t =
  Sexplib0.Sexp.
    [
      List [ Atom "version"; Atom (string_of_int last_version) ]; save (to_v1 t);
    ]
