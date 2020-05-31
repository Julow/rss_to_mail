let print_refresh = function
  | `Every h -> Printf.sprintf "(Every %f)" h
  | `At (h, m) -> Printf.sprintf "(At %d %d)" h m
  | `At_weekly (d, h, m) ->
      Printf.sprintf "(At_weekly %d %d %d)" (Utils.weekday_index d) h m

let check_next_update expected now refresh =
  let options = Feed_desc.make_options ~refresh () in
  let desc = Printf.sprintf "now=%Ld refresh=%s" now (print_refresh refresh) in
  Alcotest.(check int64) desc expected (Utils.next_update now options)

let hourly () =
  (* check_next_update 14400L 0L (`Every 4.); *)
  check_next_update 16200L 0L (`Every 4.5);
  ()

let daily () =
  check_next_update 12391200L 12304800L (* Some day, 10 AM GMT *) (`At (10, 0));
  check_next_update 12391200L 12301200L (* Some day, 9 AM GMT *) (`At (10, 0));
  check_next_update 12391200L 12308400L (* Some day, 11 AM GMT *) (`At (10, 0));
  ()

let weekly () =
  (* Next week *)
  check_next_update 12650400L 12009600L
    (* Wed, 12 AM GMT *)
    (`At_weekly (`Wed, 10, 0));
  check_next_update 12650400L 12038400L
    (* Wed, 8 AM GMT *) (`At_weekly (`Wed, 10, 0));
  (* This week *)
  check_next_update 12045600L 11872800L
    (* Wed - 2, 10 AM GMT *) (`At_weekly (`Wed, 10, 0));
  ()

let tests =
  [
    ("Hourly refresh", `Quick, hourly);
    ("Daily refresh", `Quick, daily);
    ("Weekly refresh", `Quick, weekly);
  ]
