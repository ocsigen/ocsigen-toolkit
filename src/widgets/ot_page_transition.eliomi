[%%client.start]
(** This widget helps realize page transition with screenshots. *)

(** The type of animation. [Nil] means there is no animation.
    [Forward] means going forward in history or loading a new page.
    Normally we will see a page come from the right side and cover
    the current page. [Backward] means going backward in history. In
    this case, the current page moves to the right side in order to
    display the page covered by it. *)
type animation = Nil|Forward|Backward

(** Suppose that screenshots are stored in objects of type [screenshot].
    Users need to provide a module which specifies the type screenshot
    (e.g. string) and a function [screenshot_container] that creates
    a html element from the screenshot.
*)
module type PAGE_TRANSITION_CONF = sig
  type screenshot
  val screenshot_container :
    screenshot -> Html_types.div_content Eliom_content.Html.elt
end

(** [install_global_handler t take_screenshot animation_type]
    creates a global onchangepage handler which is registered
    recursively and applies to all pages. [t] is the transition duration
    of the animation. [take_screenshot callback] takes a screenshot
    of the current page and then calls [callback]. A callback function
    is used here because [take_screenshot] is usually asynchronous.
    [animation_type ev] decides the type of the animation of page
    transition in an onchangepage event.

    TODO: we can avoid the callback function by transforming
    [take_screenshot] to a function of type [unit -> screenshot Lwt.t]*)
module Make (Conf:PAGE_TRANSITION_CONF) : sig
  type screenshot
  val install_global_handler :
    ?transition_duration : float ->
    take_screenshot: ((screenshot -> unit) -> unit) ->
    animation_type: (Eliom_client.changepage_event -> animation) -> unit
end with type screenshot = Conf.screenshot

(** [install_global_handler_withURI] enables you to skip the step of
    creating a module of type [PAGE_TRANSITION_CONF] when screenshots
    are stored as a data uri. *)
val install_global_handler_withURI :
  ?transition_duration:float ->
  take_screenshot: ((string -> unit) -> unit) ->
  animation_type: (Eliom_client.changepage_event -> animation) -> unit
