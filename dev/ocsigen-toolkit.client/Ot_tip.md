
# Module `Ot_tip`


### Tip widget

This module implement a `display` function which actually display a tip (i.e. a box over the page content).

It appends an element (which is called here "the filter") to body. The filter is meant take the whole page space, in order to capture clicks to close the tip. The filter also contains another element, called the menu which is the container for content to be displayed.

`filter_a`: filter attributes default is ` a_class ["ot-drp-filter"] ` and will be overriden if you provide this argument.

`menu_a`: menu attributes default is ` [ a_class ["ot-drp-menu"] ] ` and will be overriden if you provide this argument.

`position`: specify how the tip whould be positioned horizontally with respect to the `origin` element. By default, the tip is above the `origin` element when there is more space above than below the `origin` element and vice versa. When position is ``Forced_top` or ``Forced_bottom`, the tip is always above (resp. the below) the `origin` element. When position is ``Top` or ``Bottom`, the tip is above (resp. the below) the `origin` element unless the tip is off the screen and in this case the tip will be below (resp. the above) the `origin` element. When position is ``Ratio r`, the tip is below the `origin` element if the `origin` element is on the top `r` part of the screen otherwise the tip will be above the `origin` element.

`side`: specify how the tip whould be positioned vertically with respect to the `origin` element. By default, the tip is centered; if it would not fit on screen, its right hand side or left hand side is aligned with the middle of the `origin` element. When side is ``Left` or ``Right`, the tip and the `origin` element are aligned on the right (resp. the left).

`origin` is the element from which the tip is supposed to pop out.

`onopen filter menu side` is called after the filter is append to body.

`onclose filter menu side` is called after the tip has been closed.

`content` is the function generating the main content. It takes the function to close the tip as parameter

```ocaml
val display : 
  ?container_a:[< Html_types.div_attrib Class ] Eliom_content.Html.attrib list ->
  ?filter_a:
    [< Html_types.div_attrib Class OnClick ] Eliom_content.Html.attrib list ->
  ?position:[ `Forced_top | `Top | `Ratio of float | `Bottom | `Forced_bottom ] ->
  ?side:[ `Left | `Right | `Center ] ->
  origin:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  ?onopen:
    ([> Html_types.div ] Eliom_content.Html.elt ->
      [> Html_types.div ] Eliom_content.Html.elt ->
      unit) ->
  ?onclose:
    ([> Html_types.div ] Eliom_content.Html.elt ->
      [> Html_types.div ] Eliom_content.Html.elt ->
      unit) ->
  content:
    ((unit -> unit) ->
      [< Html_types.div_content_fun Div ] Eliom_content.Html.elt list) ->
  unit ->
  [> Html_types.div ] Eliom_content.Html.elt * (unit -> unit)
```