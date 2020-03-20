(** Ensures [f] is running at most [n] times concurrently Internally uses an
    Lwt_pool of [unit] *)
let pooled n f =
  let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
  fun x -> Lwt_pool.use pool (fun () -> f x)

exception Timeout

(** Cancel a thread after [t] seconds raising the exception [Timeout]. *)
let lwt_timeout t r =
  let timeout = Lwt.bind (Lwt_unix.sleep t) (fun () -> Lwt.fail Timeout) in
  Lwt.pick [ r; timeout ]
