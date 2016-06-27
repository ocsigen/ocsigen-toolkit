[%%client.start]

open Eliom_content.Html
open Html_types

(** whether computed CSS attribute "position" equals "sticky" *)
val is_position_sticky : 'a elt -> bool
(** try to set CSS attribute "position" to "sticky". [false] if unsuccessful *)
val set_position_sticky : 'a elt -> bool

val supports_position_sticky : 'a elt -> bool

(** whether element is sticky, either due to CSS attribute position:sticky or 
    due to position:sticky polyfill [make_sticky] *)
val is_sticky : 'a elt -> bool

(** determines how certain values of [make_sticky] are computed
    [`Fix] sets the value to a inline length when the element get stuck.
    With [`Leave] the value is not touched at all.
    [`Sync] causes the value to continuously (on window scroll/resize) be
    derived from the inline other element, meaning from the inline for
    the element fixed, and from fixed for the inline.
*)

type glue = {
  fixed : div_content D.elt;
  inline : div_content D.elt;
  dir : [`Top | `Left];
  scroll_thread: unit Lwt.t;
  resize_thread: unit Lwt.t;
}

(** position:sticky polyfill which is not supported by Chrome. It functions by
    making a clone with position:fixed of the designated element and
    continuously (window scroll/resize) monitoring the position of the element
    and the clone. The contents of the element is shifted back and forth between
    the two elements. Make sure to also apply the CSS code "position: sticky" to
    the element as this function has no effect if "position: sticky" is
    supported by the browser. The supplied element should be a D-element.
*)
val make_sticky :
  dir:[ `Left | `Top ] ->
  ?ios_html_scroll_hack:bool ->
  div_content elt -> glue option

(** stop element from being sticky *)
val dissolve : glue -> unit

type leash = {thread: unit Lwt.t; glue: glue option}

(** make sure an element gets never out of sight while scrolling by continuously
    (window scroll/resize) monitoring the position of the element and adjusting
    the top/left value. Calls [make_sticky]. Make sure to also apply the CSS
    code "position: sticky" to the element.
*)
val keep_in_sight : dir:[ `Left | `Top ] -> div_content elt -> leash option

(** stop element from being in sight (and also sticky) *)
val release : leash -> unit
