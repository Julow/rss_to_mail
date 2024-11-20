let print_feed (feed : Feed.t) =
  let opt f = function Some s -> f s | None -> "None" in
  let list f = function
    | [] -> "None"
    | lst -> String.concat ", " (List.map f lst)
  in
  let attach_to_string Feed.{ attach_url; attach_type; _ } =
    Printf.sprintf "%s (type = %s)" (Uri.to_string attach_url)
      (opt Fun.id attach_type)
  in
  let p = Printf.printf in
  let print_entry (t : Feed.entry) =
    p "- id: %s\n" (opt Fun.id t.id);
    p "  title: %s\n" (opt Fun.id t.title);
    p "  link: %s\n" (opt Uri.to_string t.link);
    p "  summary: %s\n" (opt (fun c -> c.Feed.content_text) t.summary);
    p "  thumbnail: %s\n" (opt Uri.to_string t.thumbnail);
    p "  attachments: %s\n" (list attach_to_string t.attachments);
    ()
  in
  p "Feed title: %s\n" (opt Fun.id feed.metadata.feed_title);
  p "Feed link: %s\n" (opt Uri.to_string feed.metadata.feed_link);
  p "Feed icon: %s\n" (opt Uri.to_string feed.metadata.feed_icon);
  p "Entries (%d):\n" (Array.length feed.entries);
  Array.iter print_entry feed.entries
