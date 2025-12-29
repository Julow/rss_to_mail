open Feed
open Operators

(** Atom parser * https://validator.w3.org/feed/docs/atom.html *)

let ns = "http://www.w3.org/2005/Atom"
and media_ns = "http://search.yahoo.com/mrss/"

let attr = Xml.attr
let text = Xml.text
let children_all = Xml.children_all

let children ~ns name node =
  match Xml.children ~ns name node with
  | [] -> Xml.children name node
  | cs -> cs

let ( < ) ?(ns = ns) node f =
  match ( < ) ~ns node f with Some _ as x -> x | None -> node < f

let ( << ) node name = children ~ns name node

module Links = struct
  type rel =
    | Alternate
    | Self
    | Enclosure
    | Other of string

  type t = (rel * (string * Xml.node)) list

  let rel = function
    | None | Some "alternate" -> Alternate
    | Some "self" -> Self
    | Some "enclosure" -> Enclosure
    | Some rel -> Other rel

  let get_all rel links =
    let f (rel', v) = if rel = rel' then Some v else None in
    List.filter_map f links

  let get rel links = List.assoc_opt rel links

  let rec of_nodes = function
    | [] -> []
    | node :: tl -> (
        match attr "href" node with
        | None -> of_nodes tl
        | Some href ->
            let rel = rel (attr "rel" node) in
            (rel, (href, node)) :: of_nodes tl
      )
end

let uri ~resolve_uri s = resolve_uri (Uri.of_string s)

let decode_content ~resolve_uri node =
  match attr "type" node with
  | None | Some "text" -> Some (Feed.make_text_content (text node))
  | Some "html" -> Some (Html_content.of_html_text ~resolve_uri (text node))
  | Some "xhtml" -> Some (Html_content.of_xml ~resolve_uri (children_all node))
  | Some _ -> None

let attachment ~resolve_uri (href, node) =
  {
    attach_url = uri ~resolve_uri href;
    attach_size = attr "length" node >$ Int64.of_string_opt;
    attach_type = attr "type" node;
  }

let author ~resolve_uri node =
  node < "name" > fun name ->
  {
    author_name = text name;
    author_link = node < "uri" > uri ~resolve_uri % text;
  }

let category node = { term = attr "term" node; label = attr "label" node }

let media_group ~resolve_uri entry =
  match entry <~ (media_ns, "group") with
  | None -> (None, None)
  | Some group ->
      let thumbnail =
        group <~ (media_ns, "thumbnail") >$ fun node ->
        attr "url" node > fun url ->
        {
          thumbnail_uri = uri ~resolve_uri url;
          thumbnail_width = attr "width" node >$ int_of_string_opt;
          thumbnail_height = attr "height" node >$ int_of_string_opt;
        }
      in
      (thumbnail, group <~ (media_ns, "description") > text)

let entry ~resolve_uri node =
  let links = Links.of_nodes (children ~ns "link" node) in
  let thumbnail, media_description = media_group ~resolve_uri node in
  let content =
    match node < "content" >$ decode_content ~resolve_uri with
    | Some _ as c -> c
    | None -> media_description > Feed.make_text_content
  in
  {
    id = node < "id" > text;
    title = node < "title" > text;
    summary = node < "summary" >$ decode_content ~resolve_uri;
    content;
    date = node < "updated" > text;
    link = Links.(get Alternate) links > uri ~resolve_uri % fst;
    attachments = Links.(get_all Enclosure) links >> attachment ~resolve_uri;
    thumbnail;
    authors = node << "author" >>$ author ~resolve_uri;
    categories = node << "category" >> category;
  }

let feed ~resolve_uri node =
  let links = Links.of_nodes (children ~ns "link" node) in
  let metadata =
    {
      feed_title = node < "title" > text;
      feed_icon = node < "icon" > uri ~resolve_uri % text;
      feed_link = Links.(get Alternate) links > uri ~resolve_uri % fst;
    }
  in
  { metadata; entries = node << "entry" >> entry ~resolve_uri |> Array.of_list }

let parse = feed
