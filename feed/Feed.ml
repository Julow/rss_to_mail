type category = {
  label : string option;
  term : string option;
}

type author = {
  author_name : string;
  author_link : Uri.t option;
}

type attachment = {
  attach_url : Uri.t;
  attach_size : Int64.t option;
  attach_type : string option;
}

type html_name = string * string

type html_content =
  | Html_T of string
  | Html_E of html_name * (html_name * string) list * html_content list

type content = {
  content_text : string;
  content_html : html_content list;
}

type entry = {
  id : string option;
  title : string option;
  authors : author list;
  categories : category list;
  summary : content option;
  content : content option;
  link : Uri.t option;
  thumbnail : Uri.t option;
  date : string option;
  attachments : attachment list;
}

type t = {
  feed_title : string option;
  feed_link : Uri.t option;
  feed_icon : Uri.t option;
  entries : entry array;
}

let empty_entry =
  {
    id = None;
    title = None;
    authors = [];
    categories = [];
    summary = None;
    content = None;
    link = None;
    thumbnail = None;
    date = None;
    attachments = [];
  }

(** Try to returns the ID of an entry If the ID is None, try the link, title and
    date *)
let entry_id e =
  match e.id with
  | Some _ as id -> id
  | None -> (
      match (e.link, e.title) with
      | Some link, _ -> Some (Uri.to_string link)
      | None, (Some _ as title) -> title
      | None, None -> e.date
    )

let make_text_content content_text =
  { content_text; content_html = [ Html_T content_text ] }
