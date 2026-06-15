
# Module `Ot_style`

This module is an interface to getComputedStyle.

```ocaml
val parse_px : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> float option
```
```ocaml
val float_of_px : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t -> float
```
```ocaml
val px_of_float : float -> string
```
```ocaml
val style : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.cssStyleDeclaration Js_of_ocaml.Js.t
```
```ocaml
type display = 
  | Block
  | Compact
  | Flex
  | Inherit
  | Inline
  | Inline_block
  | Inline_flex
  | Inline_table
  | Initial
  | List_item
  | Marker
  | None
  | Run_in
  | Table
  | Table_caption
  | Table_cell
  | Table_column
  | Table_column_group
  | Table_footer_group
  | Table_header_group
  | Table_row
  | Table_row_group
  | Unknown
```
```ocaml
val display_to_str : display -> string
```
```ocaml
val display_of_str : string -> display
```
```ocaml
val display : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> string
```
```ocaml
val display' : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> display
```
```ocaml
val visibility : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> string
```
```ocaml
val invisible : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> bool
```
whether an element is currently invisible in the DOM (not the screen); you might want to do a `Ot_nodeready.nodeready` before.

```ocaml
val top : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float option
```
```ocaml
val bottom : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float option
```
```ocaml
val left : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float option
```
```ocaml
val right : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float option
```
```ocaml
val marginTop : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float
```
```ocaml
val marginBottom : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float
```
```ocaml
val marginLeft : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float
```
```ocaml
val marginRight : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> float
```
```ocaml
val set_top : 'a Eliom_content.Html.elt -> float -> unit
```
```ocaml
val set_bottom : 'a Eliom_content.Html.elt -> float -> unit
```
```ocaml
val set_left : 'a Eliom_content.Html.elt -> float -> unit
```
```ocaml
val set_right : 'a Eliom_content.Html.elt -> float -> unit
```
```ocaml
val set_width : 'a Eliom_content.Html.elt -> float -> unit
```
```ocaml
val set_height : 'a Eliom_content.Html.elt -> float -> unit
```