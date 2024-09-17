(lang dune 2.8)

(name rss_to_mail)

(cram enable)

(version dev)

(generate_opam_files true)

(source
 (github Julow/rss_to_mail))

(license MIT)

(authors "Jules Aguillon <juloo.dsi@gmail.com>")

(maintainers "Jules Aguillon <juloo.dsi@gmail.com>")

(package
 (name rss_to_mail)
 (synopsis "Sends an email for every new entries on RSS feeds")
 (depends
  (ocaml
   (>= 4.10))
  uri
  xmlm
  markup
  lambdasoup
  lwt
  cmdliner
  tyxml
  (tyxml-ppx
   (>= 4.5.0))
  colombe
  astring
  sendmail
  logs
  (x509
   (>= 0.10))
  ptime
  sexplib0
  emile
  (mrmime
   (>= 0.3.2))
  ppx_sexp_conv
  (alcotest :with-test)
  (mdx :with-test)))

(package
 (name rss_to_mail-unix)
 (synopsis "Sends an email for every new entries on RSS feeds")
 (depends
  (ocaml
   (>= 4.10))
  rss_to_mail
  lwt_ssl
  lwt
  cmdliner
  logs
  fmt
  (x509
   (>= 0.10))
  ptime
  sexplib))
