
# Module type `Ot_page_transition.PAGE_TRANSITION_CONF`

Suppose that screenshots are stored in objects of type `screenshot`. Users need to provide a module which specifies the type screenshot (e.g. string) and a function `screenshot_container` that creates a html element from the screenshot.

```ocaml
type screenshot
```
```ocaml
val screenshot_container : 
  screenshot option ->
  Html_types.div_content Eliom_content.Html.elt
```