(* Ocsigen
 * http://www.ocsigen.org
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

open Js_of_ocaml
open Eliom_content.Html
open Html_types

(** An HTML element which can be selected by pressing the tab key *)
class type tabbable =
  object
    inherit Dom_html.element

    method tabIndex : int Js.prop
  end

val setup_tabcycle : #tabbable Js.t list -> unit
(** [setup_tabcycle] makes tab key loop over child elements of an element and
    only these elements. *)

val setup_tabcycle_auto : Dom_html.element Js.t -> unit
(** [setup_tabcycle_auto] scans an element for tabbable elements (buttons, inputs)
    and feeds them to [setup_tabcycle] *)

val setup_form : Dom_html.element Js.t -> unit
(** Scan for focusable elements apply [setup_tabcycle_auto] to them and
    focus the first. *)

val prevent_tab : Dom_html.element Js.t -> unit -> unit
(** [prevent_tab e] prevents [e] (and its children) to be focused with tab key.
    A function to restore the initial status is returned. *)
