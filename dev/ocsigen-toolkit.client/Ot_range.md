
# Module `Ot_range`

Range selection widget

```ocaml
val make : 
  ?txt_up:string ->
  ?txt_down:string ->
  ?f:(int -> string) ->
  ?lb:int ->
  int ->
  [> `Div ] Eliom_content.Html.elt * int Eliom_shared.React.S.t
```
`make ?txt_up ?txt_down ~f ~lb ub` produces a widget for picking one of the values in `[lb, ub)` via "up" and "down" buttons marked with the text `txt_up` and `txt_down`. `f i` provides the text displayed for the `i`\-th value, for `i` in `[lb, ub)`.
