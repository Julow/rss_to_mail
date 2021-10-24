Errors reported while parsing a config file are bad.

  $ parse_config <<EOF
  > (feed_url unknown_opt)
  > EOF
  Error: Feed "feed_url",
  option "unknown_opt":
  Unknown option "unknown_opt"

  $ parse_config <<EOF
  > (feed_url (unknown_opt foo))
  > EOF
  Error: Feed "feed_url",
  option "unknown_opt":
  Unknown option "unknown_opt"
