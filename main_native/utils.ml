(** Ensures [f] is running at most [n] times concurrently Internally uses an
    Lwt_pool of [unit] *)
let pooled n f =
  let pool = Lwt_pool.create n (fun _ -> Lwt.return_unit) in
  fun x -> Lwt_pool.use pool (fun () -> f x)

(** Cancel a thread after [t] seconds raising the exception [Timeout]. *)
let lwt_timeout fail t r =
  let timeout = Lwt.bind (Lwt_unix.sleep t) fail in
  Lwt.pick [ r; timeout ]
