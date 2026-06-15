
# Module `Ot_nodeready`

```ocaml
val nodeready : Js_of_ocaml.Dom.node Js_of_ocaml.Js.t -> unit Lwt.t
```
Wait for a node to be inserted in the DOM.

Example

`let _ = nodeready node in Console.console##debug node`

Known issues

Using it on a node that is never actually added in the DOM will make the node and the thread wakener kept in memory. Also, note that nodeready is fired only once (except if you add a new listener to it after triggering the first one).
