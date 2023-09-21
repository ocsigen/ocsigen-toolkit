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

[%%client.start]

open Js_of_ocaml

(** {2 Size functions for Dom elements}

    {3 Size and orientation} *)

type orientation = Portrait | Landscape

val get_screen_size : unit -> int * int
val get_screen_orientation : unit -> orientation

val get_size :
   < clientHeight : < get : int ; .. > Js.gen_prop
   ; clientWidth : < get : int ; .. > Js.gen_prop
   ; .. >
     Js.t
  -> int * int

val get_document_size : unit -> int * int

val width_height : (int * int) React.signal
(** NOTE: mind to stop any signals derived from the following signals (using
    [React.S.stop]) on unload. *)

val width : int React.signal
val height : int React.signal
val update_width_height : unit -> unit

val set_adaptative_width : #Dom_html.element Js.t -> (int -> int) -> unit
(** [set_adaptative_width elt f] will make the width of the element
    recomputed using [f] everytime the width of the window changes. *)

val set_adaptative_height : #Dom_html.element Js.t -> (int -> int) -> unit
(** [set_adaptative_height elt f] will make the width of the element
    recomputed using [f] everytime the height of the window changes. *)

val height_to_bottom : int -> #Dom_html.element Js.t -> int
(** Compute the height of an element to the bottom of the page *)

val client_top : ?with_margin:bool -> #Dom_html.element Js.t -> float
(** position of an element relative to the inner window;
    getClientBoundingRect does not include borders by default, use [with_margin]
    to take them into account.
*)

val client_bottom : ?with_margin:bool -> #Dom_html.element Js.t -> float
val client_left : ?with_margin:bool -> #Dom_html.element Js.t -> float
val client_right : ?with_margin:bool -> #Dom_html.element Js.t -> float

val client_height : ?with_margin:bool -> #Dom_html.element Js.t -> float
(** size of an element *)

val client_width : ?with_margin:bool -> #Dom_html.element Js.t -> float

val client_page_top : ?with_margin:bool -> #Dom_html.element Js.t -> float
(** position of an element relative to the document *)

val client_page_left : ?with_margin:bool -> #Dom_html.element Js.t -> float
val client_page_bottom : ?with_margin:bool -> #Dom_html.element Js.t -> float
val client_page_right : ?with_margin:bool -> #Dom_html.element Js.t -> float

val pageYOffset : unit -> int
(** Current vertical scroll position of the page. *)
