
# Module `Ot_swipe`

```ocaml
val bind : 
  ?transition_duration:float ->
  ?min:(unit -> int) Eliom_client_value.t ->
  ?max:(unit -> int) Eliom_client_value.t ->
  compute_final_pos:
    (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
      int ->
      int)
      Eliom_client_value.t ->
  ?onstart:
    (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
      int ->
      unit)
      Eliom_client_value.t ->
  ?onmove:
    (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
      int ->
      unit)
      Eliom_client_value.t ->
  ?onend:
    (Js_of_ocaml.Dom_html.touchEvent Js_of_ocaml.Js.t ->
      int ->
      unit)
      Eliom_client_value.t ->
  Html_types.div_content Eliom_content.Html.elt ->
  unit
```
`bind ~compute_final_pos elt` makes `elt` left-right swipable on touch screens. `compute_final_pos` is a function that will compute the final position of the element w.r.t. the position where it has been released (in pixels). Use `(fun _ p -> p)` if you want it to stay where it was released.

Use `?min` and `?max` if you want to limit the move. If outside range, events will be propagated to parent (which makes it possible to have a swipeable element inside another one).

`?onstart` and `?onmove` and `?onend` can be used to execute some side effect on touch start, touch move and touch end. The second parameter is the current move.
