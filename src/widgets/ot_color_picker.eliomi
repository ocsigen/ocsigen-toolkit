(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Enguerrand Decorne
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

(** This module implements a color picker.  *)

type t
(** Abstract type of color pickers.  *)

val make : ?width:int -> unit -> t
(** [make ?width ()] produces a color picker.  [width] is the width of
    the different canvases of the picker; its default value is
    [100].  *)

val append_at : Dom_html.element Js.t -> t -> unit
(** [append_at elt colorp] appends the color picker [colorp] to the DOM
    element [elt].  *)

val init_handler : t -> unit
(** Initializes a handler for the given color picker.  *)

val get_rgb : t -> int * int * int
(** Returns the active color in the RGB format for the given color
    picker.  *)
