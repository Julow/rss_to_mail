Errors reported while parsing a config file are bad.

  $ parse_config <<EOF
  > (feed_url unknown_opt)
  > EOF
  Error: in feed "feed_url":
  Expected option

  $ parse_config <<EOF
  > (feed_url (unknown_opt foo))
  > EOF
  Error: in feed "feed_url",
  in option "unknown_opt":
  Unknown option "unknown_opt"
