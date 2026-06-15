
# Module `Ot_color_picker`

This module implements a color picker.

```ocaml
val hsv_to_rgb : int -> float -> float -> float * float * float
```
`hsv_to_rgb h s v` converts HS(V/L) colors to RGB.

```ocaml
val make : 
  ?a:[< Html_types.div_attrib Class ] Eliom_content.Html.attrib list ->
  ?hsv:(int * float * float) ->
  ?update:(int * float * float) React.E.t Eliom_client_value.t ->
  unit ->
  [> `Div ] Eliom_content.Html.D.elt
  * (int * float * float) Eliom_shared.React.S.t
```
`make ()` produces a color picker. `?a` is an optional parameter to add html attributes to main element, by default it is the empty list `?hsv` hue, saturation and value of the initial color displayed by the color picker `?update` is an optional React event that allows to change the selected color from outside
