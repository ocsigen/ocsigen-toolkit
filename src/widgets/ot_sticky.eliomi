[%%client.start]

open Eliom_content.Html
open Html_types

(** whether computed CSS attribute "position" equals "sticky" *)
val is_position_sticky : 'a elt -> bool

(** try to set CSS attribute "position" to "sticky". [false] if
    unsuccessful *)
val set_position_sticky : 'a elt -> bool

val supports_position_sticky : 'a elt -> bool

(** whether element is sticky, either due to CSS attribute position:sticky or
    due to position:sticky polyfill [make_sticky] *)
val is_sticky : 'a elt -> bool

(** returned by [make sticky] (if position:sticky is not supported). You only
    need this value if you want to manipulate the stickiness later (as
    [keep_in_sight] does.
    [fixed]: element cloned from the element supplied to [make_sticky];
    [inline]: original element supplied to [make_sticky];
    [dir]: see [make_sticky];
    [scroll_thread]: thread that makes either [fixed] or [inline] visible,
                     depending on the scroll position;
    [resize_thread]: thread that resizes the fixed element according to the
                     inline element on window resize;
    [dissolve]: undo [make_sticky] i.e. kill [scroll_thread] and [resize_thread]
                and remove [fixed] from the DOM tree.
*)
type glue = {
  fixed : div_content D.elt;
  inline : div_content D.elt;
  dir : [`Top | `Left];
  scroll_thread : unit Lwt.t;
  resize_thread : (int * int) React.S.t;
  dissolve : unit -> unit
}

(** position:sticky polyfill which is not supported by some browsers. It
    functions by making a clone with position:fixed of the designated
    element and continuously (window scroll/resize) monitoring the
    position of the element and the clone. The contents of the element
    is shifted back and forth between the two elements. Make sure to
    also apply the CSS code "position: sticky" to the element as this
    function has no effect if "position: sticky" is supported by the
    browser. The supplied element should be a D-element.
    [dir] determines whether it sticks to the top on vertical scroll or the the
    left on horizontal scroll.

    NOTE: Do not forget to include the CSS attributes as defined in the file
    css/ot_sticky.css.

    If [?force] is [true], will apply the polyfill even if the browser supports
    sticky position (default is [false]).

    See in {!Ot_lib} for documentation of [~ios_html_scroll_hack].
*)
val make_sticky :
  dir:[ `Left | `Top ] ->
  ?ios_html_scroll_hack:bool ->
  ?force:bool ->
  div_content elt ->
  glue option Lwt.t

(** make sure an element gets never out of sight while scrolling by
    continuously (window scroll/resize) monitoring the position of the
    element and adjusting the top/left value. Calls
    [make_sticky]. Make sure to also apply the CSS code "position:
    sticky" to the element. The element's absolute position is
    determined by the parents position (which is not sticky but
    inline), so you probably want to wrap your element in a dedicated
    div. (It has to be the parent and not the element itself because
    when the element floats (is in its fixed state) we can't use its
    position for computing the right values.  Returns a function by
    which the [keep_in_sight] functionality can be stopped. *)
val keep_in_sight :
  dir:[`Left | `Top] ->
  ?ios_html_scroll_hack:bool ->
  div_content elt ->
  (unit -> unit) Lwt.t
