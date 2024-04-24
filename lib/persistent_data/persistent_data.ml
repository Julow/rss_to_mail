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
    page_contents : string Feed_map.t;
  }

  type id = Feed_id.t

  let pp_id ppf id = Format.pp_print_string ppf (Feed_id.to_string id)

  let empty =
    {
      next_update = Feed_map.empty;
      previous_entries = Feed_map.empty;
      page_contents = Feed_map.empty;
    }

  let get_field (type a) t : a Rss_to_mail.state_key -> a Feed_map.t = function
    | Next_update -> t.next_update
    | Previous_entries -> t.previous_entries
    | Page_contents -> t.page_contents

  let set_field (type a) t (v : a Feed_map.t) : a Rss_to_mail.state_key -> t =
    function
    | Rss_to_mail.Next_update -> { t with next_update = v }
    | Previous_entries -> { t with previous_entries = v }
    | Page_contents -> { t with page_contents = v }

  let get (type a) t id (key : a Rss_to_mail.state_key) : a option =
    Feed_map.find_opt id (get_field t key)

  let set (type a) t id (key : a Rss_to_mail.state_key) (v : a) =
    set_field t (Feed_map.add id v (get_field t key)) key
end

type t = {
  data : M.t;
  unsent_mails : Rss_to_mail.mail list;
}

let empty = { data = M.empty; unsent_mails = [] }

let versions =
  let open Load_chain in
  V2.(2, of_v1, load) :: V1.(1, of_v0, load) :: V0.(V0 (0, load))

let of_v2 { V2.previous_entries; next_update; page_contents; unsent_mails } =
  let make_feed_map f l =
    List.fold_left
      (fun acc (id, x) -> Feed_map.add (Feed_id.of_url id) (f x) acc)
      Feed_map.empty l
  and mail_of_v2 { V1.sender; to_; subject; body_html; body_text; timestamp } =
    { Rss_to_mail.sender; to_; subject; body_html; body_text; timestamp }
  in
  let data =
    {
      M.previous_entries = make_feed_map SeenSet.of_list previous_entries;
      next_update = make_feed_map (fun x -> x) next_update;
      page_contents = make_feed_map (fun x -> x) page_contents;
    }
  in
  { data; unsent_mails = List.map mail_of_v2 unsent_mails }

let to_v2 { data; unsent_mails } =
  let unmake_feed_map f m =
    Feed_map.fold (fun id x acc -> (Feed_id.to_string id, f x) :: acc) m []
  and mail_to_v1
      { Rss_to_mail.sender; to_; subject; body_html; body_text; timestamp } =
    { V1.sender; to_; subject; body_html; body_text; timestamp }
  in
  {
    V2.previous_entries =
      unmake_feed_map SeenSet.to_list data.M.previous_entries;
    next_update = unmake_feed_map (fun x -> x) data.next_update;
    page_contents = unmake_feed_map (fun x -> x) data.page_contents;
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
  
