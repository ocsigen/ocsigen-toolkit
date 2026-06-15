
# Module `Ot_noderesize`


### Get an event when an element's size changes


#### Known issues

This only work with elements in the DOM (maybe that the element has to be displayd, need to check this). In case of a content loaded dynamically with js, watch a parent already thereif possible or use a `onnodeready` event to attach `noderesize` listener.

Also, if the element is removed, then re-inserted in the DOM, sensor will not work anymore.

If the element to be watched is not positionned, a `position: relative` will be applied.


#### Example

```ocaml
Lwt.async (fun () ->
    let div' = (To_dom.of_element div) in
    let%lwt () = Ot_nodeready.nodeready div' in
    Ot_noderesize.noderesize (ot_noderesize.attach div) (fun () ->
      Console.console##log (Js.string "Resized") ) )
```
```ocaml
type resize_sensor
```
```ocaml
val attach : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> resize_sensor
```
```ocaml
val noderesize : ?safe:bool -> resize_sensor -> (unit -> unit) -> unit
```
When `safe` is set to `true`, `noderesize` will work whatever sized is the watched element. When set to `false` (which is the default), elements bigger than 9999px (width or height) will not detect resize, but noderesize will be more efficient (less computation/reading).

```ocaml
val detach : resize_sensor -> unit
```