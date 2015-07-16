(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Jerome Vouillon and Vasilis Papavasileiou
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

{shared{

    (** Given a handler [(get, act)], [get y m] provides the dates
        that need to be handled for month [m] of year [y]. [act y m d]
        is the client-side action to be performed for the day
        [y]:[m]:[d] (where [d] needs to be in [get y m]). *)

type handler =
  ((int -> int -> int list Lwt.t) Eliom_lib.client_value *
   (int -> int -> int -> unit) Eliom_lib.client_value)

val make :
  ?handler:handler -> unit ->
  [> Html5_types.table ] Eliom_content.Html5.elt

}}
