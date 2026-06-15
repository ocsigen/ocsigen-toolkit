
# Module `Ot_page_transition.Make`

`install_global_handler t take_screenshot animation_type` creates a global onchangepage handler which is registered recursively and applies to all pages. `t` is the transition duration of the animation. `take_screenshot callback` takes a screenshot of the current page and then calls `callback`. A callback function is used here because `take_screenshot` is usually asynchronous. `animation_type ev` decides the type of the animation of page transition in an onchangepage event.

TODO: we can avoid the callback function by transforming `take_screenshot` to a function of type `unit -> screenshot Lwt.t`


## Parameters

```ocaml
module Conf : PAGE_TRANSITION_CONF
```

## Signature

```ocaml
type screenshot = Conf.screenshot
```
```ocaml
val install_global_handler : 
  ?transition_duration:float ->
  take_screenshot:((screenshot -> unit) -> unit) ->
  animation_type:(Eliom_client.changepage_event -> animation) ->
  unit
```