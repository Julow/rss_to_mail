# RSS to Mail

Send a mail for new entries on an RSS or Atom feeds

### Config

```sexp
((smtp ("smtp server" ("user name" "password")))
 (address "destination email address")
 (feeds
  ((http://feed_url (option1 value) (option2 value) ...)
   (http://feed_url2 options ...))))
```

#### Options

- `(label "...")` Inserted into the body of the message: " with label ..."
- `(title "...")` Override the feed title
- `(refresh 1.5)` Feed is refreshed every 1 hour and 30 minutes
	The default can be controled with the `default_refresh` global option, by default 6 hours
- `(refresh (at 10:00))` Refresh every day at 10AM (24 hours format)
- `(refresh (at 14:00 wed))` Refresh every week on Wednesday at 2PM
- `(no_content true)` True or false. If true, remove the summary part of the entry.
- `(filter "...")` Filter entries by matching their title.
	Only entries with "..." in their title kept. Accepts a regular expression.
- `(filter (not "..."))` Negative filter. Keep entries that do not have "..." in their title.
- `(filter "..." (not "..."))` Keep entries that match at least one of the filter

#### Other kind of feeds

- `scraper` Custom scraper

	`((scraper http://url (scraper definition)) options ...)`

- `bundle` Concatenate entries into a single mail, sending at most one mail per refresh

	`((bundle http://url) options ...)`

### Install

Depends on `swaks`, install it with your package manager.

Then, pin this repo with opam:

```shell
opam pin add rss_to_mail git://github.com/Julow/rss_to_mail
```

### Usage

```shell
rss_to_mail [config_file]
```

By default, the config file is `feeds.sexp`.

An other file, `feed_datas.sexp`, will be created in the current directory.
