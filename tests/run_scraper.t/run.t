  $ rss_to_mail run-scraper simple.html < simple.sexp
  Feed title: T
  Feed link: None
  Feed icon: None
  Entries (3):
  - id: C
    title: C
    link: c
    summary: None
    thumbnail: None
    attachments: None
  - id: B
    title: B
    link: b
    summary: None
    thumbnail: None
    attachments: None
  - id: A
    title: A
    link: a
    summary: None
    thumbnail: None
    attachments: None

  $ rss_to_mail run-scraper advanced.html < advanced.sexp
  Feed title: Real title
  Feed link: None
  Feed icon: feed_icon
  Entries (2):
  - id: None
    title: # Title 2
    link: l2
    summary: Content
  2
  ,
  attach2
  ,
  attach2 suite
  ,
    thumbnail: thumbnail2
    attachments: attach/3 (type = None), attach/2 (type = None)
  - id: None
    title: # Title 1
    link: l1
    summary: Content
  1
  ,
  attach1
    thumbnail: thumbnail1
    attachments: attach/1 (type = None)
