(** Write to a temporary file, remove it when the scope of [f] ends. The out
    channel is automatically closed. *)
let with_temp_file ~write f =
  let path, outp = Filename.open_temp_file "rss_to_mail_tmp_" "" in
  let remove () = Sys.remove path and close () = close_out outp in
  Fun.protect ~finally:remove (fun () ->
      Fun.protect ~finally:close (fun () -> write outp);
      f path
  )

let with_open_process_in prog args f =
  let inp = Unix.open_process_args_in prog args in
  (* To use [Fun.protect], [finally] would have to kill the process. *)
  let res = f inp in
  (res, Unix.close_process_in inp)

let with_text_file html f =
  let text = Html_content.html_text_to_text html in
  let write outp = output_string outp text in
  with_temp_file ~write f

let compute a b =
  with_text_file a (fun a' ->
      with_text_file b (fun b' ->
          let diff, status =
            with_open_process_in "diff" [| "-EZbB"; a'; b' |]
              In_channel.input_all
          in
          match status with
          | WEXITED 0 -> Ok None
          | WEXITED 1 -> Ok (Some diff)
          | _ -> Error (`Msg "Command 'diff' failed")
      )
  )
