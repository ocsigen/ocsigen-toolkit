(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Vasilis Papavasileiou
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

(** Range selection widget *)

(** [make ?txt_up ?txt_down ~f ~lb ub] produces a widget for picking
    one of the values in [\[lb, ub)] via "up" and "down" buttons marked
    with the text [txt_up] and [txt_down]. [f i] provides the text
    displayed for the [i]-th value, for [i] in [\[lb, ub)]. *)
val make :
  ?txt_up : string ->
  ?txt_down : string ->
  ?f : (int -> string) ->
  ?lb : int ->
  int ->
  [> `Div ] Eliom_content.Html5.elt * int Eliom_shared.React.S.t
