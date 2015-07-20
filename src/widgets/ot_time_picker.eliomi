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
       hours and minutes. The first part of the output is the
       clock. The second part of the output is a reactive signal [(h,
       m)] where [h] are the hours and [m] the minutes that the user
       picked. *)

val make_hours_minutes :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  (int * int) Eliom_csreact.SharedReact.S.t

(** [make_hours_minutes ()] produces a clock-style time picker for
    hours and minutes. The user is first asked to pick hours, then
    minutes with a separate clock. The first part of the output is the
    clock. The second part of the output is a reactive signal [(h, m)]
    where [h] are the hours and [m] the minutes that the user
    picked. The third part of the output is a function that can be
    called to go back to hours selection. *)

val make_hours_minutes_seq :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  (int * int) Eliom_csreact.SharedReact.S.t *
  (unit -> unit) Eliom_pervasives.client_value

(** [make_hours ()] produces a clock-style hour picker.  The first
    part of the output is the clock. The second part of the output is
    a reactive signal for the hour that the user picked. *)

val make_hours :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  int Eliom_csreact.SharedReact.S.t

(** [make_hours_24h ()] produces a 24-hour version of the hour
    picker. The clock has 24 points in nested circles (in place of an
    AM/PM button). *)

val make_hours_24h :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  int Eliom_csreact.SharedReact.S.t

(** [make_hours ()] produces a clock-style hour picker.  The first
    part of the output is the clock. The second part of the output is
    a reactive signal for the minutes that the user picked. *)

val make_minutes :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  int Eliom_csreact.SharedReact.S.t

}}
