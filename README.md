# Feed aggregator

Aggregate any feeds into a single one

Runs on [Google Apps Script](https://developers.google.com/apps-script/overview) as a web app

### Config

The list of feeds is stored in a spreadsheet on Google Drive.

This file will be created automatically with 2 columns,
the first for the feed url and the second for its options

Options are formatted as a JSON dict:

| Key	| Default value	| Description	|
| ---	| ---	| ---	|
| `"cache"`	| `"sometimes"`	| Update interval: `"always"`, `"often"`, `"sometimes"`, `"daily"`, `"rarely"`	|
| `"no_content"`	| `false`	| If true, contents are removed	|
| `"label"`	| `null`	| Inserted into the contents, usefull for filtering	|

### Build

To build you will need jbuilder and js_of_ocaml 3.0.x (`opam install jbuilder js_of_ocaml{,-ppx}`)

```
jbuilder build @all
```

Upload these files to your Apps Script project:

- `_build/default/rss_to_mail.gs`
- `_build/default/main.gs`
