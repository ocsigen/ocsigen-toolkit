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

[%%shared.start]

(** This module implements a color picker.  *)

val hsv_to_rgb : int -> float -> float -> (float * float * float)
(** [hsv_to_rgb h s v] converts HS(V/L) colors to RGB. *)

val make : ?a: [< Html_types.div_attrib > `Class ] Eliom_content.Html.attrib list ->
           [> `Div ] Eliom_content.Html.D.elt * (int * float * float) Eliom_shared.React.S.t
(** [make ()] produces a color picker. *)
