((smtp (some_server))
 (address some_address)
 (feeds (
	atom.atom
	(rss.rss (refresh (at 18:00)))
	(atom.atom (label "Label") (title "Title"))
	(firefox.html
	 (scraper
	  ("#main-content > ol > li"
		(R ("> strong > a" (T (entry (T title link))))
			("> ol > li > a" (T (entry (T title link))))))))
)))
