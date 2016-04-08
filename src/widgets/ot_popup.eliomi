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
open Eliom_content.Html5
open Html5_types

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
  [> section ] elt

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
    parameter, and returning the popup content. *)
val popup :
  ?a:[< div_attrib ] attrib list
  -> ?close_button:(Html5_types.button_content elt list)
  -> ?confirmation_onclose:(unit -> bool Lwt.t)
  -> ?onclose:(unit -> unit Lwt.t)
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
  -> ?a_hcf:[< Html5_types.div_attrib ] attrib list
  -> header:[< Html5_types.header_content ] elt list
  -> buttons:([< Html5_types.button_content_fun ] elt list
              * (unit -> 'a Lwt.t)
              * string list) list
  -> [< Html5_types.div_content ] elt list
  -> 'a Lwt.t

(** Shortcut using [ask_question] for prompting the user with a question
    and returning a boolean.
    [confirm ?a question yes no]
    [a] is a traditional optional attributes to add to the popup
    [question] is the content of the popup header
    [yes] is the content of the 'yes' button (returning true)
    [no] is the content of the 'no' button (returning false) *)
val confirm :
  ?a:[< Html5_types.div_attrib ] attrib list
  -> [< header_content_fun ] elt list
  -> ([< button_content_fun ] as 'a) elt list
  -> 'a elt list
  -> bool Lwt.t
