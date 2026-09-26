# Module `Ot.Spinner`

### Spinner widget

```ocaml
val with_spinner : 
  ?a:[< Html_types.div_attrib ] Eliom.Content.Html.attrib list ->
  ?spinner:[< Html_types.div_content ] Eliom.Content.Html.elt list ->
  ?fail:(exn -> [< Html_types.div_content ] Eliom.Content.Html.elt list Lwt.t) ->
  [< Html_types.div_content ] Eliom.Content.Html.elt list Lwt.t ->
  [> `Div ] Eliom.Content.Html.elt Lwt.t
```
On client side, `with_spinner th` returns immediately a spinner while Lwt thread `th` is not finished, that will automatically be replaced by the result of `th` when finished.

On server side, it will wait for `th` to be finished before returning its result (and never display a spinner).

If you want the spinner on both sides, you can use `with_spinner_no_lwt` and `Eliom.Content.Html.C.node`.

Function `fail` will be used to display block in case an exception is raised.

Use optional argument `spinner` on client side to customize the spinner. By default it is a `div` element with classes `ot-icon-spinner` and `ot-icon-animation-spinning`. (see default stylesheet).
