let () =
  Alcotest.run "Rss_to_mail"
    [ ("uptodate", Uptodate.tests); ("html_to_text", Test_html_to_text.tests) ]
