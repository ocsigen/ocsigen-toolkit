
# Module `Ot_toggle`


### Binary toggle widget

```ocaml
val make : 
  ?init_up:bool ->
  ?up_txt:string ->
  ?down_txt:string ->
  ?update:bool React.E.t Eliom_client_value.t ->
  unit ->
  [> `Div ] Eliom_content.Html.elt * bool Eliom_shared.React.S.t
```
`make ?init_up ?up_txt ?down_txt ()` produces a binary toggle. If `init_up` is true, the toggle is originally up (default: down). The buttons for the "up" and "down" positions are marked with `up_txt` and `down_txt`. The first part of the output is the toggle, and the second part is a Boolean reactive signal, where true means "up".
