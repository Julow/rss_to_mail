type t = {
  context : string list;
  msg : string;
}

exception E of t

let raise_error msg = raise (E { context = []; msg })

let with_context c f =
  try f ()
  with E { context; msg } ->
    let context = c () :: context in
    raise (E { context; msg })

let with_context_field n f =
  with_context (fun () -> Format.asprintf "while reading field %S" n) f

let to_string =
  let open Format in
  let pp_sep fmt () = fprintf fmt ",@\n" in
  fun { context; msg } ->
    asprintf "%a:@\n%s" (pp_print_list ~pp_sep pp_print_string) context msg

let handle_error f = try f () with E t -> failwith (to_string t)
let ( let@ ) f g = f g
