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

  let lastest_version (V0 (v, _) | (v, _, _) :: _) = v
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

let versions =
  let open Load_chain in
  V2.(2, of_v1, load) :: V1.(1, of_v0, load) :: V0.(V0 (0, load))

let of_v2 { V2.previous_entries; next_update; unsent_mails } =
  let previous_entries_of_v2 acc (id, s) =
    Feed_map.add (Feed_id.of_url id) (SeenSet.of_list s) acc
  and next_update_of_v2 acc (id, t) = Feed_map.add (Feed_id.of_url id) t acc
  and mail_of_v2 { V1.sender; to_; subject; body_html; body_text; timestamp } =
    { Rss_to_mail.sender; to_; subject; body_html; body_text; timestamp }
  in
  let data =
    {
      M.previous_entries =
        List.fold_left previous_entries_of_v2 Feed_map.empty previous_entries;
      next_update = List.fold_left next_update_of_v2 Feed_map.empty next_update;
    }
  in
  { data; unsent_mails = List.map mail_of_v2 unsent_mails }

let to_v2 { data; unsent_mails } =
  let previous_entries_to_v2 id s acc =
    (Feed_id.to_string id, SeenSet.to_list s) :: acc
  and next_update_to_v2 id t acc = (Feed_id.to_string id, t) :: acc
  and mail_to_v1 { Rss_to_mail.sender; to_; subject; body_html; body_text; timestamp } =
    { V1.sender; to_; subject; body_html; body_text; timestamp }
  in
  {
    V2.previous_entries =
      Feed_map.fold previous_entries_to_v2 data.M.previous_entries [];
    next_update = Feed_map.fold next_update_to_v2 data.next_update [];
    unsent_mails = List.map mail_to_v1 unsent_mails;
  }

let load =
  let load_v version sexp = of_v2 (Load_chain.load ~version sexp versions) in
  function
  | [] -> empty
  | [ sexp ] -> load_v 0 sexp
  | Sexplib0.Sexp.[ List [ Atom "version"; Atom v ]; sexp ] ->
      load_v (int_of_string v) sexp
  | _ -> failwith "Invalid format"

let save t =
  Sexplib0.Sexp.
    [
      List
        [
          Atom "version";
          Atom (string_of_int (Load_chain.lastest_version versions));
        ];
      V2.save (to_v2 t);
    ]
  
