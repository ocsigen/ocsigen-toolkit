
# Module `Ot_lib`

```ocaml
val in_ancestors : 
  elt:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  ancestor:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  bool
```
```ocaml
val onloads : (unit -> unit) -> unit
```
```ocaml
val onresizes : 
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
NOTE: be careful when using the functions `onresizes`, `window_scroll`, and `window_scrolls`. They may be called before the new document is displayed (and thus the new window is there) and therefore may be attached to the window that is about to be replaced. In most use-cases you should have a line as follows before: let%lwt () \= Ot\_nodeready.nodeready @@ To\_dom.of\_element some\_elt in

```ocaml
val window_scroll : 
  ?use_capture:bool ->
  unit ->
  Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t Lwt.t
```
```ocaml
val window_scrolls : 
  ?ios_html_scroll_hack:bool ->
  ?use_capture:bool ->
  (Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit Lwt.t -> unit Lwt.t) ->
  unit Lwt.t
```
If `ios_html_scroll_hack` then listen on window \+ html \+ body instead of only window. On iOS (8 and 9\), in WkWebView and in Safari, some CSS properties (e.g. html`overflow:scroll;
    -webkit-overflow-scrolling: touch;`) may move the scroll event from window to html or to body. For instance, with (ON) or without (OFF) the following CSS: `html{overflow:scroll;-webkit-overflow-scrolling: touch;}` we may observe this:

```ocaml
     | capture | elements receiving the scroll events
-----+---------+-------------------------------------
OFF  |    true | window
-----+---------+-------------------------------------
OFF  |   false | window
-----+---------+-------------------------------------
ON   |    true | window + html + body
-----+---------+-------------------------------------
ON   |   false | body
-----------------------------------------------------
```
(Also, note that pure JavaScript "onscroll" attribute might be broken when ON.) It's useful to listen on html even if it's only relevant when ON \+ capture=true, because we probably want, when capture=true, to capture the event as early as possible.

```ocaml
val click_outside : 
  ?use_capture:bool ->
  ?inside:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  Js_of_ocaml.Dom_html.mouseEvent Js_of_ocaml.Js.t Lwt.t
```
`click_outside e` returns when user clicks outside element `e`. Will only catch clicks inside the element given as optional parameter `?inside` (default is `Dom_html.document##.body`).

```ocaml
module List : sig ... end
```