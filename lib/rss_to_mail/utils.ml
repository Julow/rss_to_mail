module S = Ptime.Span

let make_span ?(hour = 0) ?(min = 0) ?(sec = 0) () =
  S.of_int_s ((hour * 3600) + (min * 60) + sec)

let span_of_hour_f h =
  let hf, h = modf h in
  let hour = int_of_float h and sec = int_of_float (hf *. 3600.) in
  make_span ~hour ~sec ()

let today_at h m now =
  let _, ((h', m', s'), _) = Ptime.to_date_time now in
  S.sub (make_span ~hour:h ~min:m ()) (make_span ~hour:h' ~min:m' ~sec:s' ())

let next_day_at h m now =
  let today = today_at h m now in
  if compare today S.zero > 0 then today else S.add today (S.v (1, 0L))

let weekday_index = function
  | `Mon -> 0
  | `Tue -> 1
  | `Wed -> 2
  | `Thu -> 3
  | `Fri -> 4
  | `Sat -> 5
  | `Sun -> 6

let next_week_at day h m now =
  let today_wd = Ptime.weekday now in
  let d = ((weekday_index day - weekday_index today_wd + 6) mod 7) + 1 in
  S.add (today_at h m now) (S.v (d, 0L))

let next_update_one now refresh =
  let now_t =
    match Ptime.of_float_s (Int64.to_float now) with
    | Some t -> t
    | None -> assert false
  in
  let d =
    match refresh with
    | `Every h -> span_of_hour_f h
    | `At (h, m) -> next_day_at h m now_t
    | `At_weekly (d, h, m) -> next_week_at d h m now_t
  in
  Int64.add now (Int64.of_float (S.to_float_s d))

let next_update now options =
  match options.Feed_desc.refresh with
  | hd :: tl ->
      List.fold_left
        (fun acc r -> Int64.min acc (next_update_one now r))
        (next_update_one now hd) tl
  | [] ->
      (* Default to every 6 hours *)
      next_update_one now (`Every 6.)

let rec size s u =
  let to_s () = Int64.to_string s ^ u in
  function
  | _ when Int64.compare s 1024L < 0 -> to_s ()
  | [] -> to_s ()
  | u' :: tl -> size (Int64.div s 1024L) u' tl

let size s = size s "b" [ "Kb"; "Mb"; "Gb" ]

let rec list_interleave elt = function
  | [] -> []
  | [ _ ] as last -> last
  | hd :: tl -> hd :: elt :: list_interleave elt tl

(** Ensures [f] is running at most [n] times concurrently Internally uses an
    Lwt_pool of [unit] *)
let pooled n f =
  let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
  fun x -> Lwt_pool.use pool (fun () -> f x)
