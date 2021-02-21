module H = struct
  (** Some functions to construct [html_content] values. *)

  let txt s = Feed.Html_T s

  let attr name value = (("", name), value)

  let e tag ?(attrs = []) childs = Feed.Html_E (("", tag), attrs, childs)
end

let check_convert ?(msg = "converts to") input expected =
  Alcotest.(check string) msg expected (Html_to_text.convert input)

let text () =
  check_convert H.[ txt "Foo bar" ] "Foo bar";
  check_convert H.[ txt "Foo"; txt "bar" ] "Foo bar";
  check_convert H.[ e "p" [ txt "Foo"; e "span" [ txt "bar" ] ] ] "Foo bar";
  check_convert
    H.[ txt "Foo"; e "a" ~attrs:[ attr "href" "url" ] [ txt "bar" ] ]
    "Foo bar <url>";
  ()

let lists () =
  check_convert ~msg:"List elements separated by newlines, list is a paragraph."
    H.
      [
        txt "List:";
        e "ul" [ e "li" [ txt "Foo" ]; e "li" [ txt "Bar" ] ];
        txt ".";
      ]
    "List:\n\n- Foo\n- Bar\n\n.";
  ()

let paragraphs () =
  check_convert
    H.[ e "p" []; e "p" [ txt "Foo"; e "p" [ e "p" []; txt "Bar" ] ]; e "p" [] ]
    "Foo\n\nBar";
  ()

let tests =
  [
    ("Text", `Quick, text);
    ("Lists", `Quick, lists);
    ("Paragraphs", `Quick, paragraphs);
  ]
