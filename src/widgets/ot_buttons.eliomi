[%%shared.start]

open Html_types
open Eliom_content.Html

(* [dropdown ~menu content] creates a dropdown button with [content] as the
   button's content and [menu] as the menu's content. Note that the menu is
   displayed when the button is being hovered over with the mouse (on mobile
   devices: when tapped on) and functions purely by CSS. *)
val dropdown :
  ?a:div_attrib attrib list ->
  menu:div_content elt list ->
  div_content elt list ->
  div_content elt
