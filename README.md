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

Options:

- `label` Inserted into the body of the message: " with label ..."
- `cache` Update frequency. May be a numeric value, in hour
	or `always`, `often`, `sometimes`, `daily`, `rarely`
- `no_content` True or false. If true, the content of entries will not be included in the mail
- `scraper` If set, run a custom scraper instead

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

### Apps Script version

It can also run on [Google Apps Script](https://developers.google.com/apps-script/overview).

The config is stored in a spreadsheet on Google Drive.

This file will be created automatically with 2 columns: the feed url and its options.

Options are formatted as a JSON dict.
