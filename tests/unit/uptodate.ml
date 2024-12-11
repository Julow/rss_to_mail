let ( ++ ) = Int64.add
let ( -- ) = Int64.sub

let print_refresh = function
  | `Every h -> Printf.sprintf "(Every %f)" h
  | `At (h, m) -> Printf.sprintf "(At %d %d)" h m
  | `At_weekly (d, h, m) ->
      Printf.sprintf "(At_weekly %d %d %d)" (Utils.weekday_index d) h m

let print_refreshes l = String.concat "," (List.map print_refresh l)

let check_next_update expected now refresh =
  let options = Feed_desc.make_options ~refresh () in
  let desc =
    Printf.sprintf "now=%Ld refresh=%s" now (print_refreshes refresh)
  in
  Alcotest.(check int64) desc expected (Utils.next_update now options)

let hourly () =
  (* check_next_update 14400L 0L (`Every 4.); *)
  check_next_update 16200L 0L [ `Every 4.5 ];
  ()

let daily () =
  check_next_update 12391200L 12304800L
    (* Some day, 10:00 UTC *) [ `At (10, 0) ];
  check_next_update 12304800L 12301200L (* Some day, 9:00 UTC *) [ `At (10, 0) ];
  check_next_update 12391200L 12308400L
    (* Some day, 11:00 UTC *) [ `At (10, 0) ];
  ()

let weekly () =
  (* Next week *)
  check_next_update 12650400L 12009600L
    (* Wed, 12:00 UTC *)
    [ `At_weekly (`Wed, 10, 0) ];
  check_next_update 12650400L 12038400L
    (* Wed, 8:00 UTC *) [ `At_weekly (`Wed, 10, 0) ];
  (* This week *)
  check_next_update 12045600L 11872800L
    (* Mon, 10:00 UTC *) [ `At_weekly (`Wed, 10, 0) ];
  ()

let multiple () =
  let sat_1000am = 12304800L
  and sat_1030am = 12306600L
  and sun_1000am = 12391200L in
  let r = [ `At (10, 0); `At (10, 30) ] in
  check_next_update sat_1000am (sat_1000am -- 1L) r;
  check_next_update sat_1030am (sat_1030am -- 1L) r;
  check_next_update sat_1030am (sat_1000am ++ 1L) r;
  check_next_update sun_1000am (sat_1030am ++ 1L) r;
  let r = [ `At (10, 30); `At (10, 0) ] in
  check_next_update sat_1000am (sat_1000am -- 1L) r;
  check_next_update sat_1030am (sat_1030am -- 1L) r;
  check_next_update sat_1030am (sat_1000am ++ 1L) r;
  check_next_update sun_1000am (sat_1030am ++ 1L) r;
  ()

let tests =
  [
    ("Hourly refresh", `Quick, hourly);
    ("Daily refresh", `Quick, daily);
    ("Weekly refresh", `Quick, weekly);
    ("Multiple refreshes", `Quick, multiple);
  ]
