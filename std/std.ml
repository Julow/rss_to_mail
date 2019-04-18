module Option =
struct

  let get default t = match t with Some v -> v | None -> default
  let map f t = match t with Some v -> Some (f v) | None -> None
  let map_or default f t = match t with Some v -> f v | None -> default
  let flat_map f t = match t with Some v -> f v | None -> None
  let or_ ~else_ t = match t with Some _ as t -> t | None -> else_
  let to_list t = match t with Some v -> [ v ] | None -> []
  let iter f t = match t with Some v -> f v | None -> ()

end

module List =
struct

  include List

  let rec assoc_all key = function
    | (key', v) :: tl when key' = key ->
      v :: assoc_all key tl
    | _ :: tl	-> assoc_all key tl
    | []		-> []

  let rec filter_map f = function
    | hd :: tl ->
      begin match f hd with
        | Some v -> v :: filter_map f tl
        | None -> filter_map f tl
      end
    | [] -> []

  let rec find_map f = function
    | hd :: tl ->
      begin match f hd with
        | Some _ as r -> r
        | None -> find_map f tl
      end
    | [] -> None

end

module Array =
struct

  include Array

  let filter f a =
    let rec loop acc i =
      if i < 0
      then acc
      else
        let e = a.(i) and i = i - 1 in
        if f e
        then loop (e :: acc) i
        else loop acc i
    in
    of_list (loop [] (length a))

  let map f a =
    init (length a) (fun i -> f a.(i))

end

module String =
struct

  include String

  let hash : string -> int = Hashtbl.hash

end

module StringMap = Map.Make (String)

let _Some v = Some v

let (%) f g x = f (g x)

let id x = x

let printf = Printf.printf
let eprintf = Printf.eprintf
let sprintf = Format.asprintf
