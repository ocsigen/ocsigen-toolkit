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

(** [ popup ?a ?enable_scrolling_hack
       ?close_button ?confirmation_onclose ?onclose gen_content ]
    Display a modal popup.
    Returns the popup container, in case you need it.

    [enable_scrolling_hack] (default: [true]) toggle the hack setting
    popup background (body) to [fixed] when popups open

    Use [close_button] if you want to add a button to close the popup.

    [confirmation_on_close] is used to ask confirmation to the user
    when closing the popup with the close button. Only relevant if
    one of [close_button], [close_on_background_click], [close_on_escape]
    is supplied.

    [onclose] is a hook called just after the popup has been actually closed.

    [gen_content] is a function taking the function closing the popup as
    parameter, and returning the popup content.

    If [close_on_background_click] (default: false) is true then clicking on the
    background of the popup closes it.

    If [close_on_escape] (default: true if [close_button] is supplied) then
    hitting the escape key will close the popup.
*)
val popup :
  ?a:[< div_attrib ] attrib list
  -> ?enable_scrolling_hack:bool
  -> ?close_button:(button_content elt list)
  -> ?confirmation_onclose:(unit -> bool Lwt.t)
  -> ?onclose:(unit -> unit Lwt.t)
  -> ?close_on_background_click:bool
  -> ?close_on_escape:bool
  -> ((unit -> unit Lwt.t) -> [< div_content ] elt Lwt.t)
  -> [> `Div ] elt Lwt.t

(** [ask_question ?a ?a_hcf question buttons]
    Prompt a user, wait for its response and return the selected value.
    [question] is the content of the popup header
    [buttons] is the list of available answers. Each button is a triple
    of [(content, action, classes)]. [action ()] is called to return the
    value when the corresponding button is clicked. *)
val ask_question :
  ?a:[< div_attrib ] attrib list
  -> ?a_hcf:[< div_attrib ] attrib list
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
  -> [< header_content_fun ] elt list
  -> ([< button_content_fun ] as 'a) elt list
  -> 'a elt list
  -> bool Lwt.t
