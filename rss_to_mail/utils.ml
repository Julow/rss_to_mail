module C = CalendarLib.Calendar.Precise

let next_day_at h m t =
  let t = C.next t `Day in
  C.create (C.to_date t) (C.Time.(make h m (Second.from_int 0)))

let day_equal a b = C.Date.int_of_day a = C.Date.int_of_day b

let next_week_at day h m t =
  let rec loop t =
    if day_equal (C.day_of_week t) day
    then t
    else loop (C.next t `Day)
  in
  loop (next_day_at h m t)

let is_uptodate now last_update options =
  let last_update = C.from_unixfloat (Int64.to_float last_update)
  and now = C.from_unixfloat (Int64.to_float now) in
  let due =
    match options.Feed_desc.refresh with
    | `Every h				->
      let hour = int_of_float h and second = int_of_float (h /. 60.) in
      C.add last_update (C.Period.lmake ~hour ~second ())
    | `At (h, m)			-> next_day_at h m last_update
    | `At_weekly (d, h, m)	-> next_week_at d h m last_update
  in
  C.compare due now > 0

let rec size s u =
  let to_s () = Int64.to_string s ^ u in
  function
  | _ when Int64.compare s 1024L < 0 -> to_s ()
  | []				-> to_s ()
  | u' :: tl			-> size (Int64.div s 1024L) u' tl

let size s = size s "b" [ "Kb"; "Mb"; "Gb" ]

let rec list_interleave elt = function
  | [] -> []
  | [ _ ] as last -> last
  | hd :: tl -> hd :: elt :: list_interleave elt tl
