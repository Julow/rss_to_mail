# RSS to Mail

An OCaml program that fetches RSS feeds and sends emails.
Many [options](https://github.com/Julow/rss_to_mail/blob/master/rss_to_mail/feed_desc.ml#L25) can be set on each feed,
including refresh interval, regex filters or different destination adresses.

The program updates feeds that need to be updated and then exits. It must be called regularly to become useful, for example, with a systemd timer.

### Usage

```shell
rss_to_mail check-config [CONFIG]
   Check the configuration file for errors and exit

rss_to_mail run [CONFIG]
   Fetch a list of feeds and send a mail for new entries

rss_to_mail run-scraper SRC
   Run a scraper against a web page. Useful for degugging. Read the
   scraper definition from stdin.
   SRC is a path or an url

rss_to_mail send-test-email
   Send a test email and exit.
```

By default, the config file is `./feeds.sexp`.

An other file, `feed_datas.sexp`, will be created in the current directory.

#### Example config

``` sexp
((smtp
   (server mymail.org 465)
   (from alice@mymail.org)
   (auth alice "mypassword"))
 (to alice@mymail.org)
 (default_refresh 4.)
 (feeds
   (https://xkcd.com/atom.xml))
)
```
