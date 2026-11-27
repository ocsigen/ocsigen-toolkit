# Module `Ot.Nodeready`

```ocaml
val nodeready : Js_of_ocaml.Dom.node Js_of_ocaml.Js.t -> unit Lwt.t
```
Wait for a node to be inserted in the DOM.

Example

`let _ = nodeready node in Console.console##debug node`

Known issues

If the node is never added to the DOM, the promise is fulfilled when the page it was created for dies (see `Eliom.Client.Page_status`), and the code waiting for it then runs on a node that is not in the DOM. Until then, the node is kept in memory. A page whose DOM is kept in the history (`Eliom.Client.push_history_dom`), or whose generation was abandoned, may never die. Also, note that nodeready is fired only once: if the node is removed and added again, call it again.
