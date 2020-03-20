open Feed
open Xml
open Operators

(** RSS 2.0 parser https://validator.w3.org/feed/docs/rss2.html *)

let dc_ns = "http://purl.org/dc/elements/1.1/"

and content_ns = "http://purl.org/rss/1.0/modules/content/"

let uri ~resolve_uri s = resolve_uri (Uri.of_string s)

let content_encoded ~resolve_uri content =
  Some (Html (Html_content.parse ~resolve_uri content))

let author author_name = { author_name; author_link = None }

let category label = { term = None; label = Some label }

let attachment ~resolve_uri node =
  attr "url" node > fun url ->
  {
    attach_url = uri ~resolve_uri url;
    attach_size = attr "length" node >$ Int64.of_string_opt;
    attach_type = attr "type" node;
  }

let entry ~resolve_uri node =
  {
    title = node < "title" > text;
    link = node < "link" > uri ~resolve_uri % text;
    id = node < "guid" > text;
    summary = node < "description" > text >$ content_encoded ~resolve_uri;
    content =
      ( < ) node ~ns:content_ns "encoded" > text >$ content_encoded ~resolve_uri;
    categories = node << "category" >> category % text;
    authors = ( << ) node ~ns:dc_ns "creator" >> author % text;
    attachments = node << "enclosure" >>$ attachment ~resolve_uri;
    date = node < "pubDate" > text;
    thumbnail = None;
  }

let feed ~resolve_uri node =
  {
    feed_title = node < "title" > text;
    feed_link = node < "link" > uri ~resolve_uri % text;
    feed_icon = node < "image" >$ child "url" > uri ~resolve_uri % text;
    entries = node << "item" >> entry ~resolve_uri |> Array.of_list;
  }

let parse ~resolve_uri node = feed ~resolve_uri @@ child_exn "channel" node
