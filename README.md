# Feed aggregator

Aggregate any feeds into a single one

Runs on [Google Apps Script](https://developers.google.com/apps-script/overview) as a web app

### Config

The list of feeds is stored in a spreadsheet on Google Drive.

This file will be created automatically with 2 columns,
the first for the feed url and the second for its options

### Build

To build you will need jbuilder and js_of_ocaml 3.0.x (`opam install jbuilder js_of_ocaml{,-ppx}`)

```
jbuilder build @all
```

Upload these files to your Apps Script project:

- `_build/default/rss_to_mail.gs`
- `_build/default/main.gs`
