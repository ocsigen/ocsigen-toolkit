
# Module `Ot_page_transition`

```ocaml
type animation = 
  | Nil
  | Forward
  | Backward
```
The type of animation. `Nil` means there is no animation. `Forward` means going forward in history or loading a new page. Normally we will see a page come from the right side and cover the current page. `Backward` means going backward in history. In this case, the current page moves to the right side in order to display the page covered by it.

```ocaml
module type PAGE_TRANSITION_CONF = sig ... end
```
Suppose that screenshots are stored in objects of type `screenshot`. Users need to provide a module which specifies the type screenshot (e.g. string) and a function `screenshot_container` that creates a html element from the screenshot.

```ocaml
module Make (Conf : PAGE_TRANSITION_CONF) : sig ... end
```
`install_global_handler t take_screenshot animation_type` creates a global onchangepage handler which is registered recursively and applies to all pages. `t` is the transition duration of the animation. `take_screenshot callback` takes a screenshot of the current page and then calls `callback`. A callback function is used here because `take_screenshot` is usually asynchronous. `animation_type ev` decides the type of the animation of page transition in an onchangepage event.

```ocaml
val install_global_handler_withURI : 
  ?transition_duration:float ->
  take_screenshot:((string -> unit) -> unit) ->
  animation_type:(Eliom_client.changepage_event -> animation) ->
  unit
```
`install_global_handler_withURI` enables you to skip the step of creating a module of type `PAGE_TRANSITION_CONF` when screenshots are stored as a data uri.
