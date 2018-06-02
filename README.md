# RSS to Mail

Send a mail for new entries on an RSS or Atom feeds

Runs on [Google Apps Script](https://developers.google.com/apps-script/overview)

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

```shell
jbuilder build
```

Dependencies:

- jbuilder, js_of_ocaml{,-ppx}, uri, containers
- https://github.com/Julow/ocaml-uri/tree/optional_sexplib

Upload this file to your Apps Script project:

- `_build/default/main_script/rss_to_mail.gs`

Setup a trigger for `doUpdate` every 10 minutes
