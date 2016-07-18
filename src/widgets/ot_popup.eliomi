(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015
 *      Vincent Balat
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

(** Popup widget *)

open Eliom_content.Html
open Html_types

(** Section with header, content and footer.
    [header] and [footer] are empty by default
    This is just a short
    Header and footer can be empty (default) and
    have fix size. Content has scrollbar if too high. *)
val hcf :
  ?a:[< div_attrib ] attrib list ->
  ?header:[< header_content_fun ] elt list ->
  ?footer:[< footer_content_fun ] elt list ->
  [< div_content ] elt list ->
  [> `Section ] elt

[%%client.start]

(** [ popup ?a ?closeable ?confirmation_onclose ?onclose gen_content ]
    Display a modal popup.
    Returns the popup container, in case you need it.
    [closeable], if true (default), display a button to close the popup
    [confirmation_on_close] is used to ask confirmation to the user
    when closing the popup with the close button. Meaningless if not
    [closeable].
    [onclose] is a hook called just after the popup has been actually closed.
    [gen_content] is a function taking the function closing the popup as
    parameter, and returning the popup content.
    For [ios_scroll_pos_fix] see [Ot_drawer.drawer].
    If [disable_background] (default: true) is true then the tabIndex of all
    the elements not in the popup are set to -1 with the effect that they can
    not be selected using the TAB key. When the popup is closed their old
    tabIndex value is restored. Note, that some elements that are tabbable in
    some browsers but not by specification (scrollable div's) are not affected.
    If [setup_form] (default: false) is true then the popup is scanned for a
    form element and [setup_tabcycle_auto] is applied. If no form element is
    found, the whole popup is scanned for form elements. *)
val popup :
  ?a:[< div_attrib ] attrib list
  -> ?close_button:(button_content elt list)
  -> ?confirmation_onclose:(unit -> bool Lwt.t)
  -> ?onclose:(unit -> unit Lwt.t)
  -> ?disable_background:bool
  -> ?setup_form:[`OnPopup | `OnSignal of bool React.S.t]
  -> ?ios_scroll_pos_fix:bool
  -> ((unit -> unit Lwt.t) -> [< div_content ] elt Lwt.t)
  -> [> `Div ] elt Lwt.t

val resetup_form_signal :
  unit -> [> `OnSignal of bool React.S.t] * (unit -> unit Lwt.t)

(** [ask_question ?a ?a_hcf question buttons]
    Prompt a user, wait for its response and return the selected value.
    [question] is the content of the popup header
    [buttons] is the list of available answers. Each button is a triple
    of [(content, action, classes)]. [action ()] is called to return the
    value when the corresponding button is clicked. *)
val ask_question :
  ?a:[< div_attrib ] attrib list
  -> ?a_hcf:[< div_attrib ] attrib list
  -> ?disable_background:bool
  -> ?setup_form:[`OnPopup | `OnSignal of bool React.S.t]
  -> header:[< header_content ] elt list
  -> buttons:([< button_content_fun ] elt list
              * (unit -> 'a Lwt.t)
              * string list) list
  -> [< div_content ] elt list
  -> 'a Lwt.t

(** Shortcut using [ask_question] for prompting the user with a question
    and returning a boolean.
    [confirm ?a question yes no]
    [a] is a traditional optional attributes to add to the popup
    [question] is the content of the popup header
    [yes] is the content of the 'yes' button (returning true)
    [no] is the content of the 'no' button (returning false) *)
val confirm :
  ?a:[< div_attrib ] attrib list
  -> ?disable_background:bool
  -> ?setup_form:[`OnPopup | `OnSignal of bool React.S.t]
  -> [< header_content_fun ] elt list
  -> ([< button_content_fun ] as 'a) elt list
  -> 'a elt list
  -> bool Lwt.t

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
