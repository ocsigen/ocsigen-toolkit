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

   (** [make_hours_minutes ?round_5 ()] produces a clock-style time
       picker for hours and minutes. If [round_5] is true (default:
       false), the output for the minutes is rounded to multiples of
       5. The first part of the output is the clock. The second part
       of the output is a reactive signal [(h, m)] where [h] are the
       hours and [m] the minutes that the user picked. *)

val make_hours_minutes :
  ?round_5 : bool ->
  unit ->
  [> Html5_types.div] Eliom_content.Html5.D.elt *
  (int * int) Eliom_csreact.SharedReact.S.t

(** [make_hours_minutes_seq ?action ?round_5 ?h24 ()] produces a
    clock-style time picker for hours and minutes. The user is first
    asked to pick hours, then minutes with a separate clock. If
    [action] is provided, it is called when a new time is
    available. If [round_5] is true (default: false), the output for
    the minutes is rounded to multiples of 5. If [h24] is true
    (default: true), a 24-hour hour picker is used. The first part of
    the output is the clock. The second part of the output is a
    reactive signal [(h, m)] where [h] are the hours and [m] the
    minutes that the user picked. The third part of the output is a
    function that can be called to go back to hours selection. *)

val make_hours_minutes_seq :
  ?action :
    (int * int -> unit Lwt.t) Eliom_pervasives.client_value ->
  ?round_5 :
    bool ->
  ?h24 :
    bool ->
  unit ->
  [> Html5_types.div] Eliom_content.Html5.D.elt *
  (int * int) Eliom_csreact.SharedReact.S.t *
  (unit -> unit) Eliom_pervasives.client_value

(** [make_hours ?h24 ()] produces a clock-style hour picker. If [h24]
    is true, a 24-hour version is produced (i.e., 24 points in nested
    circles). The first part of the output is the clock. The second
    part of the output is a reactive signal for the hour that the user
    picked. *)

val make_hours :
  ?h24 : bool ->
  unit ->
  [> Html5_types.div] Eliom_content.Html5.D.elt *
  int Eliom_csreact.SharedReact.S.t

(** [make_minutes ?round_5 ()] produces a clock-style minute picker.
    If [round_5] is true (default: false), the output is rounded to
    multiples of 5. The first part of the output is the clock. The
    second part of the output is a reactive signal for the minutes
    that the user picked. *)

val make_minutes :
  ?round_5 : bool ->
  unit ->
  [> Html5_types.div] Eliom_content.Html5.D.elt *
  int Eliom_csreact.SharedReact.S.t

}}
