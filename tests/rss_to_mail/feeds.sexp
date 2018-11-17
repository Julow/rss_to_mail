((smtp (some_server))
 (address some_address)
 (default_refresh 2)
 (feeds (
	atom.atom
	(rss.rss (refresh (at 18:00)))
	(rss.rss (label "Label") (title "Title") (bundle true))
	(firefox.html
	 (scraper
	  ("#main-content > ol > li"
		(R ("> strong > a" (T (entry (T title link))))
			("> ol > li > a" (T (entry (T title link))))))) (refresh 12.))
)))
