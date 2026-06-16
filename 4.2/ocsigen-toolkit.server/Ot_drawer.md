
# Module `Ot_drawer`


### Drawer menu for mobile and Web applications

```ocaml
val drawer : 
  ?a:[< Html_types.div_attrib ] Eliom_content.Html.attrib list ->
  ?position:[ `Top | `Right | `Bottom | `Left ] ->
  ?opened:bool ->
  ?swipe:bool ->
  ?onclose:(unit -> unit) Eliom_client_value.t ->
  ?onopen:(unit -> unit) Eliom_client_value.t ->
  ?wrap_close:
    ((unit -> unit) Eliom_client_value.t ->
      (unit -> unit) Eliom_client_value.t) ->
  ?wrap_open:
    ((unit -> unit) Eliom_client_value.t ->
      (unit -> unit) Eliom_client_value.t) ->
  [< Html_types.div_content ] Eliom_content.Html.elt list ->
  [> `Div ] Eliom_content.Html.elt
  * (unit -> unit) Eliom_client_value.t
  * (unit -> unit) Eliom_client_value.t
```
Build a drawer menu on the left, right, top or bottom of the screen. Returns the DOM element, and functions to open and close the menu. It is also possible to open or close the menu by clicking on a button, and to swipe the menu to close it.

If `opened` is true (false by default), the drawer is initialized in its opened state.

If `swipe` is true (default), the user can swipe to open or close the drawer.

If present, function `onclose` is called just after the drawer is closed, and function `onopen` just before it starts opening.

`wrap_close` and `wrap_open` can be used to customize calls to `close` and `open_` functions when the drawer closing or opening is triggered. For example, drawer closing can be disabled using `wrap_close`.
