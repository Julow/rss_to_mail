let default_opts = Feed_desc.make_options ()

let () =
  let sexp = Sexplib.Sexp.input_sexp stdin in
  try ignore @@ Feeds_config.parse_feed ~default_opts sexp
  with Failure msg -> Format.eprintf "Error: %s@\n" msg
