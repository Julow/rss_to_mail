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

- `label` Inserted into the body of the message: " with label ..."
- `title` Override the feed title
- `refresh` Update frequency.
	Maybe in hours (`(refresh 2.)`, every 2 hours)
	or fixed (`(refresh (at 18:00))`, every days at 6PM)
	The default can be controled with the `default_refresh` global option, by default 6 hours
- `no_content` True or false. If true, the content of entries will not be included in the mail
- `bundle` If true, new entries will be concatenated in a single mail
- `filter` Filter entries by matching their title.
	`(filter "abc")` Only show entries that have "abc" in their title.
	`(filter (not "abc"))` Only show entries that do not have "abc" in their title.
	If multiple filters are set, entries are shown if at least one filter match, `(filter (not "abc") "def")` only show entries that do not match "abc" and/or match "def".

#### Other kind of feeds

- `scraper` Custom scraper

	`((scraper http://url (scraper definition)) options ...)`

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
