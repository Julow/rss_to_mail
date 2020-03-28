# Test persistent data

```ocaml
(* Prelude *)
#require "containers.sexp";;
#require "rss_to_mail.persistent_data";;
#require "rss_to_mail";;

let pp_seen_set ppf : SeenSet.t -> unit =
  let pp_elem ppf id removed () =
    match removed with
    | Some time -> Format.fprintf ppf "(%S %LdL)@ " id time
    | None -> Format.fprintf ppf "%S@ " id
  in
  let pp ppf set = SeenSet.fold (pp_elem ppf) set () in
  Format.fprintf ppf "@[<hv 2>[ %a]@]" pp

let pp_string_map pp_a ppf : 'a Persistent_data.StringMap.t -> unit =
  let pp_binding ppf key a = Format.fprintf ppf "%s: %a@ " key pp_a a in
  let pp_map ppf m = Persistent_data.StringMap.iter (pp_binding ppf) m in
  Format.fprintf ppf "@[<hv 2>{ %a}@]" pp_map

;;
#install_printer pp_seen_set;;
#install_printer pp_string_map;;
```

## Config

```ocaml
let config_input = {|
(
 (smtp
  (server server 465)
  (from from@address)
  (auth id password))
 (to to@address)
 (default_refresh 4.)
 (feeds
  (feed_1
   (feed_2)
   (feed_3 (refresh 3.)) ; Refresh every 3 hours
   (feed_4 (refresh (at 17:00))) ; Refresh every day at 5PM
   (feed_5 (refresh (at 17:00 wed))) ; Refresh every week on wednesday at 5PM
   (feed_6 (title "Title") (label "Label") (no_content true))
   (feed_7 (filter "filter1" ("filter2") (not "filter3")))

   ((bundle bundle_1))
   ((bundle bundle_2) (refresh 2.))

   ((scraper scraper_1
     ("sel1"
      (T
       (entry
        (R
         ("sel11" (T id title link))
         ("sel12" (T summary))))))
     ("sel2" (T feed_title feed_icon))
   ))

   (with-options ((refresh 1.) (label "Label"))
    with_opts_1
    (with_opts_2 (refresh 0.))
    (with_opts_3 (label "Label2")))
  )
 )
)
|}

let config_input = Result.get_ok (CCSexp.parse_string config_input)
```

```ocaml
# Persistent_data.load_feeds config_input ;;
- : Persistent_data.config =
{Persistent_data.server = ("server", 465);
 server_auth = `Plain ("id", "password"); from_address = "from@address";
 to_address = "to@address";
 feeds =
  [(Feed_desc.Feed "feed_1",
    {Feed_desc.refresh = `Every 4.; title = None; label = None;
     no_content = false; filter = []});
   (Feed_desc.Feed "feed_2",
    {Feed_desc.refresh = `Every 4.; title = None; label = None;
     no_content = false; filter = []});
   (Feed_desc.Feed "feed_3",
    {Feed_desc.refresh = `Every 3.; title = None; label = None;
     no_content = false; filter = []});
   (Feed_desc.Feed "feed_4",
    {Feed_desc.refresh = `At (17, 0); title = None; label = None;
     no_content = false; filter = []});
   (Feed_desc.Feed "feed_5",
    {Feed_desc.refresh = `At_weekly (CalendarLib.Date.Wed, 17, 0);
     title = None; label = None; no_content = false; filter = []});
   (Feed_desc.Feed "feed_6",
    {Feed_desc.refresh = `Every 4.; title = Some "Title";
     label = Some "Label"; no_content = true; filter = []});
   (Feed_desc.Feed "feed_7",
    {Feed_desc.refresh = `Every 4.; title = None; label = None;
     no_content = false;
     filter = [(<abstr>, true); (<abstr>, true); (<abstr>, false)]});
   (Feed_desc.Bundle "bundle_1",
    {Feed_desc.refresh = `Every 4.; title = None; label = None;
     no_content = false; filter = []});
   (Feed_desc.Bundle "bundle_2",
    {Feed_desc.refresh = `Every 2.; title = None; label = None;
     no_content = false; filter = []});
   (Feed_desc.Scraper ("scraper_1",
     Scrap.R
      [("sel1",
        Scrap.T
         [Scraper.Entry
           [Scrap.R
             [("sel11", Scrap.T [Scraper.Id; Scraper.Title; Scraper.Link]);
              ("sel12", Scrap.T [Scraper.Summary])]]]);
       ("sel2", Scrap.T [Scraper.Feed_title; Scraper.Feed_icon])]),
    {Feed_desc.refresh = `Every 4.; title = None; label = None;
     no_content = false; filter = []});
   (Feed_desc.Feed "with_opts_1",
    {Feed_desc.refresh = `Every 1.; title = None; label = Some "Label";
     no_content = false; filter = []});
   (Feed_desc.Feed "with_opts_2",
    {Feed_desc.refresh = `Every 0.; title = None; label = Some "Label";
     no_content = false; filter = []});
   (Feed_desc.Feed "with_opts_3",
    {Feed_desc.refresh = `Every 1.; title = None; label = Some "Label2";
     no_content = false; filter = []})]}
```

## Feed datas

```ocaml
let feed_datas_input = {|
((feed_data
  ((feed_1 1234567890
    (id_1
     (id_2 1234567890)

    )
  )))
 (unsent
  ((sender subject "body_html")
   (sender subject "body_html" "body_text")
   ((sender sender)
    (subject subject)
    (body_html "body_html")
    (body_text "body_text"))

  )))
|}

let feed_datas_input = Result.get_ok (CCSexp.parse_string feed_datas_input)
let feed_datas = Persistent_data.load_feed_datas feed_datas_input
```

Parsed:

```ocaml
# feed_datas ;;
- : Persistent_data.feed_datas =
{Persistent_data.feed_datas =
  { feed_1: (1234567890L, [ "id_1" ("id_2" 1234567890L) ]) };
 unsent_mails =
  [{Rss_to_mail.sender = "sender"; subject = "subject";
    body_html = "body_html"; body_text = ""};
   {Rss_to_mail.sender = "sender"; subject = "subject";
    body_html = "body_html"; body_text = "body_text"};
   {Rss_to_mail.sender = "sender"; subject = "subject";
    body_html = "body_html"; body_text = "body_text"}]}
```

Printed:

```ocaml
# Persistent_data.save_feed_datas feed_datas |> Format.printf "%a@\n" CCSexp.pp ;;
((feed_data ((feed_1 1234567890 ((id_2 1234567890) id_1))))
 (unsent
  (((sender sender) (subject subject) (body_html body_html) (body_text ))
   ((sender sender) (subject subject) (body_html body_html)
    (body_text body_text))
   ((sender sender) (subject subject) (body_html body_html)
    (body_text body_text)))))
- : unit = ()
```
