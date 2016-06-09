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
    For [ios_scroll_pos_fix] see [Ot_drawer.drawer] *)
val popup :
  ?a:[< div_attrib ] attrib list
  -> ?close_button:(button_content elt list)
  -> ?confirmation_onclose:(unit -> bool Lwt.t)
  -> ?onclose:(unit -> unit Lwt.t)
  -> ?ios_scroll_pos_fix:bool
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

val setup_form :
  (< appendChild : Dom.node Js.t -> Dom.node Js.t Js.meth;
     childNodes : Dom.node Dom.nodeList Js.t Js.prop;
     cloneNode : bool Js.t -> Dom.node Js.t Js.meth;
     compareDocumentPosition : Dom.node Js.t ->
                               Dom.DocumentPosition.t Js.meth;
     firstChild : Dom.node Js.t Js.opt Js.prop;
     focus : 'jsoo_4beb75c4 Js.meth;
     hasChildNodes : bool Js.t Js.meth;
     insertBefore : Dom.node Js.t ->
                    Dom.node Js.t Js.opt -> Dom.node Js.t Js.meth;
     lastChild : Dom.node Js.t Js.opt Js.prop;
     lookupNamespaceURI : Js.js_string Js.t ->
                          Js.js_string Js.t Js.opt Js.meth;
     lookupPrefix : Js.js_string Js.t ->
                    Js.js_string Js.t Js.opt Js.meth;
     namespaceURI : Js.js_string Js.t Js.opt Js.prop;
     nextSibling : Dom.node Js.t Js.opt Js.prop;
     nodeName : Js.js_string Js.t Js.readonly_prop;
     nodeType : Dom.nodeType Js.readonly_prop;
     nodeValue : Js.js_string Js.t Js.opt Js.readonly_prop;
     onclick : ('a Js.t, Dom_html.mouseEvent Js.t)
               Dom_html.event_listener Js.writeonly_prop;
     ondblclick : ('a Js.t, Dom_html.mouseEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     ondrag : ('a Js.t, Dom_html.dragEvent Js.t)
              Dom_html.event_listener Js.writeonly_prop;
     ondragend : ('a Js.t, Dom_html.dragEvent Js.t)
                 Dom_html.event_listener Js.writeonly_prop;
     ondragenter : ('a Js.t, Dom_html.dragEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     ondragleave : ('a Js.t, Dom_html.dragEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     ondragover : ('a Js.t, Dom_html.dragEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     ondragstart : ('a Js.t, Dom_html.dragEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     ondrop : ('a Js.t, Dom_html.dragEvent Js.t)
              Dom_html.event_listener Js.writeonly_prop;
     onkeydown : ('a Js.t, Dom_html.keyboardEvent Js.t)
                 Dom_html.event_listener Js.writeonly_prop;
     onkeypress : ('a Js.t, Dom_html.keyboardEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     onkeyup : ('a Js.t, Dom_html.keyboardEvent Js.t)
               Dom_html.event_listener Js.writeonly_prop;
     onmousedown : ('a Js.t, Dom_html.mouseEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     onmousemove : ('a Js.t, Dom_html.mouseEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     onmouseout : ('a Js.t, Dom_html.mouseEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     onmouseover : ('a Js.t, Dom_html.mouseEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     onmouseup : ('a Js.t, Dom_html.mouseEvent Js.t)
                 Dom_html.event_listener Js.writeonly_prop;
     onscroll : ('a Js.t, Dom_html.event Js.t)
                Dom_html.event_listener Js.writeonly_prop;
     parentNode : Dom.node Js.t Js.opt Js.prop;
     previousSibling : Dom.node Js.t Js.opt Js.prop;
     removeChild : Dom.node Js.t -> Dom.node Js.t Js.meth;
     replaceChild : Dom.node Js.t ->
                    Dom.node Js.t -> Dom.node Js.t Js.meth;
     tabIndex : < set : int -> unit; .. > Js.gen_prop; .. >
   as 'a)
  Js.t ->
  < tabIndex : < set : int -> unit; .. > Js.gen_prop; .. > Js.t ->
  < tabIndex : < set : int -> unit; .. > Js.gen_prop; .. > Js.t ->
  (< onclick : ('b Js.t, Dom_html.mouseEvent Js.t)
               Dom_html.event_listener Js.writeonly_prop;
     ondblclick : ('b Js.t, Dom_html.mouseEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     ondrag : ('b Js.t, Dom_html.dragEvent Js.t)
              Dom_html.event_listener Js.writeonly_prop;
     ondragend : ('b Js.t, Dom_html.dragEvent Js.t)
                 Dom_html.event_listener Js.writeonly_prop;
     ondragenter : ('b Js.t, Dom_html.dragEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     ondragleave : ('b Js.t, Dom_html.dragEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     ondragover : ('b Js.t, Dom_html.dragEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     ondragstart : ('b Js.t, Dom_html.dragEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     ondrop : ('b Js.t, Dom_html.dragEvent Js.t)
              Dom_html.event_listener Js.writeonly_prop;
     onkeydown : ('b Js.t, Dom_html.keyboardEvent Js.t)
                 Dom_html.event_listener Js.writeonly_prop;
     onkeypress : ('b Js.t, Dom_html.keyboardEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     onkeyup : ('b Js.t, Dom_html.keyboardEvent Js.t)
               Dom_html.event_listener Js.writeonly_prop;
     onmousedown : ('b Js.t, Dom_html.mouseEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     onmousemove : ('b Js.t, Dom_html.mouseEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     onmouseout : ('b Js.t, Dom_html.mouseEvent Js.t)
                  Dom_html.event_listener Js.writeonly_prop;
     onmouseover : ('b Js.t, Dom_html.mouseEvent Js.t)
                   Dom_html.event_listener Js.writeonly_prop;
     onmouseup : ('b Js.t, Dom_html.mouseEvent Js.t)
                 Dom_html.event_listener Js.writeonly_prop;
     onscroll : ('b Js.t, Dom_html.event Js.t)
                Dom_html.event_listener Js.writeonly_prop;
     tabIndex : < set : int -> unit; .. > Js.gen_prop; .. >
   as 'b)
  Js.t -> unit
