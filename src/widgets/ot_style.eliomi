[%%client.start]

(** This module is an interface to getComputedStyle. *)

open Js_of_ocaml
open Eliom_content.Html

val parse_px : Js.js_string Js.t -> float option
val float_of_px : Js.js_string Js.t -> float
val px_of_float : float -> string
val style : #Dom_html.element Js.t -> Dom_html.cssStyleDeclaration Js.t

(* -------------------------------------------------------------------------- *)
(* Sum type for the display property.
 * See http://www.w3schools.com/jsref/prop_style_display.asp.
 *)
type display =
  | Block
  | Compact
  | Flex
  | Inherit
  | Inline
  | Inline_block
  | Inline_flex
  | Inline_table
  | Initial
  | List_item
  | Marker
  | None
  | Run_in
  | Table
  | Table_caption
  | Table_cell
  | Table_column
  | Table_column_group
  | Table_footer_group
  | Table_header_group
  | Table_row
  | Table_row_group
  | Unknown

val display_to_str : display -> string
val display_of_str : string -> display

(* -------------------------------------------------------------------------- *)

val display : #Dom_html.element Js.t -> string
val display' : #Dom_html.element Js.t -> display
val visibility : #Dom_html.element Js.t -> string

val invisible : #Dom_html.element Js.t -> bool
(** whether an element is currently invisible in the DOM (not the screen);
    you might want to do a [Ot_nodeready.nodeready] before. *)

val top : #Dom_html.element Js.t -> float option
val bottom : #Dom_html.element Js.t -> float option
val left : #Dom_html.element Js.t -> float option
val right : #Dom_html.element Js.t -> float option
val marginTop : #Dom_html.element Js.t -> float
val marginBottom : #Dom_html.element Js.t -> float
val marginLeft : #Dom_html.element Js.t -> float
val marginRight : #Dom_html.element Js.t -> float
val set_top : 'a elt -> float -> unit
val set_bottom : 'a elt -> float -> unit
val set_left : 'a elt -> float -> unit
val set_right : 'a elt -> float -> unit
val set_width : 'a elt -> float -> unit
val set_height : 'a elt -> float -> unit
