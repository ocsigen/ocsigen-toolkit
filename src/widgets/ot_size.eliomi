(* Ocsigen-toolkit
 * http://www.ocsigen.org/ocsigen-toolkit
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Charly Chevalier
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Size functions for Dom elements. *)

[%%client.start]
  (** {3 Size and orientation} *)

type orientation = Portrait | Landscape

val get_screen_size : unit -> int * int

val get_screen_orientation : unit -> orientation

val get_size :
  < clientHeight : < get : int; .. > Js.gen_prop;
    clientWidth : < get : int; .. > Js.gen_prop; .. > Js.t ->
 int * int

val get_document_size : unit -> int * int

val width_height : (int * int) React.signal
val width : int React.signal
val height : int React.signal
val update_width_height : unit -> unit

(** [set_adaptative_width elt f] will make the width of the element
    recomputed using [f] everytime the width of the window changes. *)
val set_adaptative_width : #Dom_html.element Js.t -> (int -> int) -> unit

(** [set_adaptative_height elt f] will make the width of the element
    recomputed using [f] everytime the height of the window changes. *)
val set_adaptative_height : #Dom_html.element Js.t -> (int -> int) -> unit

(** Compute the height of an element to the bottom of the page *)
val height_to_bottom : int -> #Dom_html.element Js.t -> int

(** position of an element relative to the inner window;
    getClientBoundingRect does not include borders by default, use [with_border]
    to take them into account.
*)
val client_top    : ?with_border:bool -> #Dom_html.element Js.t -> float
val client_bottom : ?with_border:bool -> #Dom_html.element Js.t -> float
val client_left   : ?with_border:bool -> #Dom_html.element Js.t -> float
val client_right  : ?with_border:bool -> #Dom_html.element Js.t -> float
(** size of an element *)
val client_height : ?with_border:bool -> #Dom_html.element Js.t -> float
val client_width  : ?with_border:bool -> #Dom_html.element Js.t -> float

(** position of an element relative to the document *)
val client_page_top  : ?with_border:bool -> #Dom_html.element Js.t -> float
val client_page_left : ?with_border:bool -> #Dom_html.element Js.t -> float

(** Current vertical scroll position of the page. *)
val pageYOffset : unit -> int
