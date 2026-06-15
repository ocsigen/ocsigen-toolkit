
# Module `Ot_popup`

Popup widget

```ocaml
val hcf : 
  ?a:[< Html_types.div_attrib ] Eliom_content.Html.attrib list ->
  ?header:[< Html_types.header_content_fun ] Eliom_content.Html.elt list ->
  ?footer:[< Html_types.footer_content_fun ] Eliom_content.Html.elt list ->
  [< Html_types.div_content ] Eliom_content.Html.elt list ->
  [> `Section ] Eliom_content.Html.elt
```
Section with header, content and footer. `header` and `footer` are empty by default This is just a short Header and footer can be empty (default) and have fix size. Content has scrollbar if too high.
