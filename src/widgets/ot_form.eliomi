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

open Eliom_content.Html
open Html_types

(** An HTML element which can be selected by pressing the tab key *)
class type tabbable = object
  inherit Dom_html.element
  method tabIndex : int Js.prop
end

(** [setup_tabcycle] makes a form in a popup more user-friendly, by focussing on
    the first element of the form and forcing tab keys to cycle through the
    elements of the form only (and not the elements of the page behind the
    popup). Note: you get proper tab cycles only for three or more elements! The
    list does not need to be complete, as only the first, the second, the next
    to last, and the last element matter. *)
val setup_tabcycle : #tabbable Js.t list -> unit Lwt.t

(** [setup_tabcycle_auto] scans an element for tabbable elements (buttons, inputs)
    and feeds them to [setup_tabcycle] *)
val setup_tabcycle_auto : Dom_html.element Js.t -> unit Lwt.t

(** The popup is scanned for a form element
    and [setup_tabcycle_auto] is applied to it (if no form element is
    found the whole popup is scanned). This happens either once the popup opens
    (if [setup_form] equals [`OnPopup]) or with [`OnSignal] the tabcycling can
    be switched on (popup is rescanned) and off with a boolean signal.

    If [disable_rest] (default: true) is true then the tabIndex of all
    the elements not in the popup are set to -1 with the effect that they can
    not be selected using the TAB key. When the popup is closed their old
    tabIndex value is restored. Note, that some elements that are tabbable in
    some browsers but not by specification (scrollable div's) are not affected.
    A function to cancel this behavior is returned.
*)

val setup_form
  : trigger:[ `OnNodeReady | `OnSignal of bool React.S.t]
  -> ?modal:bool
  -> Dom_html.element Js.t
  -> (unit -> unit) option Lwt.t

val resetup_form_signal
  : unit -> [> `OnSignal of bool React.S.t] * (unit -> unit Lwt.t)
