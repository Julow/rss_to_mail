module List = struct
  include List

  let rec assoc_all key = function
    | (key', v) :: tl when key' = key -> v :: assoc_all key tl
    | _ :: tl -> assoc_all key tl
    | [] -> []
end

module StringMap = Map.Make (String)

let ( % ) f g x = f (g x)
