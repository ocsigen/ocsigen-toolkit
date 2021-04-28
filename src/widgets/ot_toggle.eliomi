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

(** {2 Binary toggle widget} *)

val make
  :  ?init_up:bool
  -> ?up_txt:string
  -> ?down_txt:string
  -> ?update:bool React.E.t Eliom_client_value.t
  -> unit
  -> [> `Div] Eliom_content.Html.elt * bool Eliom_shared.React.S.t
(** [make ?init_up ?up_txt ?down_txt ()] produces a binary toggle. If
    [init_up] is true, the toggle is originally up (default: down). The
    buttons for the "up" and "down" positions are marked with [up_txt]
    and [down_txt]. The first part of the output is the toggle, and the
    second part is a Boolean reactive signal, where true means "up". *)
