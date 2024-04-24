let with_text_file html f =
  let text = Html_content.html_text_to_text html in
  let write outp = output_string outp text in
  Utils.with_temp_file ~write f

let compute a b =
  with_text_file a (fun a' ->
      with_text_file b (fun b' ->
          let diff, status =
            Utils.with_open_process_in "diff" [| "-EZbB"; a'; b' |]
              In_channel.input_all
          in
          match status with
          | WEXITED 0 -> Ok None
          | WEXITED 1 -> Ok (Some diff)
          | _ -> Error (`Process_error "Command 'diff' failed")
      )
  )
