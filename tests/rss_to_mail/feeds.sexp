((smtp (some_server))
 (address some_address)
 (default_refresh 2)
 (feeds (
	(atom.atom (filter "title"))
	./atom.atom
	(rss.rss (refresh (at 18:00)))
	(./rss.rss (refresh (at 18:00)))
	(././rss.rss (label "Label") (title "Title") (bundle true) (filter "2"))
	(./././rss.rss (filter (not "Title") "2"))
	(firefox.html
	 (scraper
	  ("#main-content > ol > li"
		(R ("> strong > a" (T (entry (T title link))))
			("> ol > li > a" (T (entry (T title link))))))) (refresh 12.))
	(empty.rss (bundle true))
	no_title.rss
	content_type.atom
)))
