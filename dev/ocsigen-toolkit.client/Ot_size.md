
# Module `Ot_size`


### Size functions for Dom elements


#### Size and orientation

```ocaml
type orientation = 
  | Portrait
  | Landscape
```
```ocaml
val get_screen_size : unit -> int * int
```
```ocaml
val get_screen_orientation : unit -> orientation
```
```ocaml
val get_size : 
  < clientHeight : < get : int.. > Js_of_ocaml.Js.gen_prop
    ; clientWidth : < get : int.. > Js_of_ocaml.Js.gen_prop.. >
    Js_of_ocaml.Js.t ->
  int * int
```
```ocaml
val get_document_size : unit -> int * int
```
```ocaml
val width_height : (int * int) React.signal
```
NOTE: mind to stop any signals derived from the following signals (using `React.S.stop`) on unload.

```ocaml
val width : int React.signal
```
```ocaml
val height : int React.signal
```
```ocaml
val update_width_height : unit -> unit
```
```ocaml
val set_adaptative_width : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  (int -> int) ->
  unit
```
`set_adaptative_width elt f` will make the width of the element recomputed using `f` everytime the width of the window changes.

```ocaml
val set_adaptative_height : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  (int -> int) ->
  unit
```
`set_adaptative_height elt f` will make the width of the element recomputed using `f` everytime the height of the window changes.

```ocaml
val height_to_bottom : 
  int ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  int
```
Compute the height of an element to the bottom of the page

```ocaml
val client_top : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
position of an element relative to the inner window; getClientBoundingRect does not include borders by default, use `with_margin` to take them into account.

```ocaml
val client_bottom : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
```ocaml
val client_left : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
```ocaml
val client_right : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
```ocaml
val client_height : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
size of an element

```ocaml
val client_width : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
```ocaml
val client_page_top : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
position of an element relative to the document

```ocaml
val client_page_left : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
```ocaml
val client_page_bottom : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
```ocaml
val client_page_right : 
  ?with_margin:bool ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  float
```
```ocaml
val pageYOffset : unit -> int
```
Current vertical scroll position of the page.
