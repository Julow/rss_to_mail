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
  (cohttp-lwt-unix
   (>= 6.0.0~beta2))
  cmdliner
  tyxml
  (tyxml-ppx
   (>= 4.5.0))
  colombe
  (sendmail-lwt
   (>= 0.10.0))
  logs
  (x509
   (>= 1.0.2))
  ca-certs
  ptime
  sexplib0
  sexplib
  emile
  (mrmime
   (>= 0.3.2))
  (alcotest :with-test)
  (mdx :with-test)))
