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

(** This module implements a clock-style time picker. *)

val make :
   ?action:(int * int -> unit) Eliom_client_value.t
  -> ?init:int * int
  -> ?update:(int * int) React.E.t Eliom_client_value.t
  -> ?round_5:bool
  -> ?h24:bool
  -> unit
  -> [> `Div] Eliom_content.Html.elt
     * (int * int) Eliom_shared.React.S.t
     * (unit -> unit) Eliom_client_value.t
(** [make ?action ?round_5 ?h24 ()] produces a clock-style time picker
    for hours and minutes. The user is first asked to pick hours, then
    minutes with a separate clock.

    If [action] is provided, it is called when a new time is
    available.

    [init] (if provided) is the default displayed time.

    [update] is a React event that can be used to trigger updates from
    outside.

    If [round_5] is true (default: false), the output for the minutes
    is rounded to multiples of 5.

    If [h24] is true (default: true), a 24-hour hour picker is
    used.

    The first part of the output is the clock. The second part of the
    output is a reactive signal [(h, m)] where [h] are the hours and
    [m] the minutes that the user picked. The third part of the output
    is a function that can be called to go back to hours selection. *)

val make_hours_minutes_seq :
   ?action:(int * int -> unit) Eliom_client_value.t
  -> ?init:int * int
  -> ?update:(int * int) React.E.t Eliom_client_value.t
  -> ?round_5:bool
  -> ?h24:bool
  -> unit
  -> [> `Div] Eliom_content.Html.elt
     * (int * int) Eliom_shared.React.S.t
     * (unit -> unit) Eliom_client_value.t
(** Alias of [make]. Deprecated. *)
