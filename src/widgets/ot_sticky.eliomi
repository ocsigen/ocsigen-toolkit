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
    [`Fix] sets the value to a fixed length when the element get stuck.
    With [`Leave] the value is not touched at all.
    [`Sync] causes the value to continuously (on window scroll/resize) be
    derived from the placeholder other element, meaning from the placeholder for
    the element elt, and from elt fro the placeholder.
*)
type set_size = [ `Fix | `Leave | `Sync ]

type glue = {
  elt : div_content D.elt;
  placeholder : div_content D.elt;
  dir : [`Top | `Left];
  pos: set_size;
  placeholder_width: set_size;
  placeholder_height: set_size;
  elt_width: set_size;
  elt_height: set_size;
  scroll_thread: unit Lwt.t;
  resize_thread: unit Lwt.t;
}

(** polyfill for the value "sticky" of the CSS position attribute which is not
    supported by Chrome. It functions by creating a clone ("placeholder") of the
    designated element and continuously (window scroll/resize) monitoring the
    position of the element and the placeholder.
    [placeholder_width], [placeholder_height], [elt_width], [elt_height]
    determine the size of the placeholder and the element while the element is
    stuck; be careful not to create cycles (e.g. [`Fix] for both
    [placeholder_width] and [elt_width]).
    Make sure to also apply the CSS code "position: sticky" to the element as
    this function has no effect if "position: sticky" is supported by the
    browser (TODO)
*)
val make_sticky :
  dir:[ `Left | `Top ] ->
  ?placeholder_width:set_size ->
  ?placeholder_height:set_size ->
  ?elt_width:set_size ->
  ?elt_height:set_size ->
  ?pos:set_size ->
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
