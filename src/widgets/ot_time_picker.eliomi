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

{shared{

    (** [make_hours_minutes ()] produces a clock-style time picker for
        hours and minutes. The output is a reactive signal [(h, m)]
        where [h] are the hours and [m] the minutes that the user
        picked. *)

val make_hours_minutes :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  (int * int) React.signal Eliom_pervasives.client_value

(** [make_hours_minutes ()] produces a clock-style time picker for
    hours and minutes. The user is first asked to pick hours, then
    minutes with a separate clock. The output is a reactive signal
    [(h, m)] where [h] are the hours and [m] the minutes that the user
    picked. *)

val make_hours_minutes_seq :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  (int * int) React.signal Eliom_pervasives.client_value

(** [make_hours f] produces a clock-style hour picker. *)

val make_hours :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  int React.signal Eliom_pervasives.client_value

val make_hours_24h :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  int React.signal Eliom_pervasives.client_value

(** [make_minutes f] produces a clock-style minute picker. *)

val make_minutes :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  int React.signal Eliom_pervasives.client_value

}}
