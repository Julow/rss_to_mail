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
  lwt_ssl
  lwt
  cohttp-lwt-unix
  cmdliner
  tyxml
  (tyxml-ppx
   (>= 4.5.0))
  (colombe
   (= 0.4.0))
  sendmail-lwt
  logs
  (x509
   (>= 0.10))
  ptime
  cstruct-unix
  sexplib0
  sexplib
  emile
  (mrmime
   (>= 0.3.2))
  (alcotest :with-test)
  (mdx :with-test)))
