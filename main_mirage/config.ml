open Mirage

let main =
  foreign "Unikernel.Main" job

let () =
  register "rss_to_mail" [main]
