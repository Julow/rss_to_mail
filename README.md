# RSS to Mail

Send a mail for new entries on RSS and Atom feeds

### Config

```sexp
((smtp
   (server "smtp server" port)
   (from "sender email address")
   (auth "username" "password"))
 (to "destination email address")
 (feeds
  ((http://feed_url (option1 value) (option2 value) ...)
   (http://feed_url2 options ...)
   (with-options ((option1 value) ...)
    feed1
    (feed2 options ...)))))
```

#### Options

- `(label "...")` Inserted in the header line: " with label ..."
- `(title "...")` Override the feed title
- `(refresh 1.5)` Feed is refreshed every 1 hour and 30 minutes
	The default can be controled with the `default_refresh` global option, by default 6 hours
- `(refresh (at 10:00))` Refresh every day at 10AM (24 hours format)
- `(refresh (at 14:00 wed))` Refresh every week on Wednesday at 2PM
- `(no_content true)` True or false. If true, remove the summary part of the entry.
- `(filter expr)` Filter entries using regexp. `expr` can be:
  `(and expr...)`, `(or expr...)`, `(not expr)`, `(title "regex")`, `(content "regex")`

#### Other kind of feed

- `scraper` Parse entries from HTML documents

	`((scraper http://url (scraper definition)) options ...)`

- `bundle` Concatenate entries into a single mail, sending at most one mail per refresh

	`((bundle http://url) options ...)`

#### Example config

``` sexp
((smtp
   (server mymail.org 465)
   (from alice@mymail.org)
   (auth alice mypassword))
 (to alice@mymail.org)
 (default_refresh 4.)
 (feeds
   (https://xkcd.com/atom.xml))
)
```

### Install

Using OPAM:

```shell
opam pin add rss_to_mail https://github.com/Julow/rss_to_mail.git
```

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
