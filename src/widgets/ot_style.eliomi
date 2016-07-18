[%%client.start]

(** This module is an interface to getComputedStyle *)

open Eliom_content.Html

val parse_px : Js.js_string Js.t -> float option
val float_of_px : Js.js_string Js.t -> float
val px_of_float : float -> string

val style : #Dom_html.element Js.t -> Dom_html.cssStyleDeclaration Js.t

val display : #Dom_html.element Js.t -> string
val visibility : #Dom_html.element Js.t -> string

(** whether an element is currently invisible in the DOM (not the screen);
    you might want to do a [Ot_nodeready.nodeready] before. *)
val invisible : #Dom_html.element Js.t -> bool

val top    : #Dom_html.element Js.t -> float option
val bottom : #Dom_html.element Js.t -> float option
val left   : #Dom_html.element Js.t -> float option
val right  : #Dom_html.element Js.t -> float option

val marginTop    : #Dom_html.element Js.t -> float
val marginBottom : #Dom_html.element Js.t -> float
val marginLeft   : #Dom_html.element Js.t -> float
val marginRight  : #Dom_html.element Js.t -> float

val set_top    : 'a elt -> float -> unit
val set_bottom : 'a elt -> float -> unit
val set_left   : 'a elt -> float -> unit
val set_right  : 'a elt -> float -> unit
val set_width  : 'a elt -> float -> unit
val set_height : 'a elt -> float -> unit
