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

module Feed_id : sig
  type t

  val of_url : string -> t
  val to_string : t -> string
  val compare : t -> t -> int
end = struct
  type t = string

  let of_url url = url
  let to_string url = url
  let compare = String.compare
end

module Feed_map = Map.Make (Feed_id)

module M = struct
  type t = {
    next_update : int64 Feed_map.t;
    previous_entries : SeenSet.t Feed_map.t;
  }

  type id = Feed_id.t

  let empty =
    { next_update = Feed_map.empty; previous_entries = Feed_map.empty }

  let get (type a) t id (key : a Rss_to_mail.state_key) : a option =
    match key with
    | Rss_to_mail.Next_update -> Feed_map.find_opt id t.next_update
    | Previous_entries -> Feed_map.find_opt id t.previous_entries

  let set (type a) t id (key : a Rss_to_mail.state_key) (v : a) =
    match key with
    | Next_update -> { t with next_update = Feed_map.add id v t.next_update }
    | Previous_entries ->
        { t with previous_entries = Feed_map.add id v t.previous_entries }
end

type t = {
  data : M.t;
  unsent_mails : Rss_to_mail.mail list;
}

let empty = { data = M.empty; unsent_mails = [] }

let last_version, save, versions =
  Load_chain.(V1.(lastest 1 of_v0 ~load ~save) V0.(V0 (0, load)))

let of_v1 { V1.feed_datas; unsent_mails } =
  let mail_of_v1 { V1.sender; to_; subject; body_html; body_text; timestamp } =
    Rss_to_mail.{ sender; to_; subject; body_html; body_text; timestamp }
  in
  let add_data acc (key, next_update, seen_ids) =
    let id = Feed_id.of_url key and seen_ids = SeenSet.of_list seen_ids in
    {
      M.next_update = Feed_map.add id next_update acc.M.next_update;
      previous_entries = Feed_map.add id seen_ids acc.previous_entries;
    }
  in
  {
    data = List.fold_left add_data M.empty feed_datas;
    unsent_mails = List.map mail_of_v1 unsent_mails;
  }

let to_v1 { data; unsent_mails } =
  let data_to_v1 id next_update acc =
    let previous_entries = Feed_map.find id data.previous_entries in
    (Feed_id.to_string id, next_update, SeenSet.to_list previous_entries) :: acc
  in
  let mail_to_v1 { Rss_to_mail.sender; to_; subject; body_html; body_text; timestamp } =
    { V1.sender; to_; subject; body_html; body_text; timestamp }
  in
  V1.
    {
      feed_datas = Feed_map.fold data_to_v1 data.next_update [] |> List.rev;
      unsent_mails = List.map mail_to_v1 unsent_mails;
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
  
