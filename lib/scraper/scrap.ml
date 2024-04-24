type 'a t =
  | R of (string * 'a t) list
  | T of 'a list

(** Consider nodes in the order in which they appear in the page rather than in
    the order the rules appear in the scraper definition. *)
module Position = struct
  type t = int list

  let rec _of_node n =
    match Soup.parent n with
    | Some n' -> Soup.index_of n :: _of_node n'
    | None -> []

  let of_node n = List.rev (_of_node n)

  let rec compare a b =
    match (a, b) with
    | [], [] -> 0
    | a :: atl, b :: btl -> if a = b then compare atl btl else a - b
    | [], _ :: _ -> 1
    | _ :: _, [] -> -1
end

(** Evaluate selectors and sort the elements in the order in which they appear
    in the page. Each result is associated with its corresponding sub-rule. *)
let eval_rules node rules =
  List.fold_left
    (fun acc (sel, rule') ->
      let nodes' = Soup.select sel node in
      Soup.fold
        (fun acc node -> (Position.of_node node, (node, rule')) :: acc)
        acc nodes'
    )
    [] rules
  |> List.sort (fun (a, _) (b, _) -> Position.compare b a (* reversed *))
  |> List.rev_map snd

let rec scrap node f acc = function
  | R rules -> scrap_rules node f acc rules
  | T targets -> List.fold_left (f node) acc targets

and scrap_rules node f acc rules =
  let rules = eval_rules node rules in
  List.fold_left (fun acc (node, rule') -> scrap node f acc rule') acc rules
