(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015 BeSport, Julien Sagot
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

(** Wait for a node to be inserted in the DOM. *)

(** {3 Known issues}
   Using it on a node that is never actually added in the DOM
   will make the node and the thread wakener kept in memory.
   Also, note that nodeready is fired only once (except if you add a new
   listener to it after triggering the first one).
*)

(** {3 Example}
    [let _ = nodeready node in Firebug.console##debug node]
*)

[%%client.start]

val nodeready : #Dom.node Js.t -> unit Lwt.t
