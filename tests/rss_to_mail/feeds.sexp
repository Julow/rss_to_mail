((smtp
  (server dummy)
  (from from_addr)
  (auth login password)
 )
 (to some_address)
 (default_refresh 2)
 (feeds (
	(atom.atom (filter (title "title")))
	./atom.atom
	(rss.rss (refresh (at 18:00)))
	((bundle ././rss.rss) (label "Label") (title "Title") (filter (title "2")))
	(./././rss.rss (filter (and (or (title "Title")) (title "2"))))
	((bundle empty.rss))
	no_title.rss
	content_type.atom
  (with-options ((label "Lbl"))
   ./empty.rss
   ././empty.rss)
  https://some-website/relative.atom
  https://some-website/relative.rss
  (./no_title.rss (filter (title "Title")))
  (././no_title.rss (filter (not (title "Title"))))
  (././././rss.rss (filter (content "summary")))
  (./././././rss.rss (filter (content "description")))
  (././././././rss.rss (filter (not (content "description"))))
  (./././././././rss.rss (max_entries 2))
  (././././././././rss.rss (content remove))
  ((diff simple.html))
  error.rss
  bad_xmlns.atom
)))
