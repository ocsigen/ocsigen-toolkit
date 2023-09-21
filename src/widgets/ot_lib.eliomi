(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015-09
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

[%%client.start]

open Js_of_ocaml

val in_ancestors :
   elt:Dom_html.element Js.t
  -> ancestor:Dom_html.element Js.t
  -> bool

val onloads : (unit -> unit) -> unit

val onresizes : (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t
(** NOTE: be careful when using the functions [onresizes],
    [window_scroll], and [window_scrolls]. They may be called before
    the new document is displayed (and thus the new window is there)
    and therefore may be attached to the window that is about to be
    replaced. In most use-cases you should have a line as follows
    before: let%lwt () = Ot_nodeready.nodeready @@ To_dom.of_element
    some_elt in *)

val window_scroll : ?use_capture:bool -> unit -> Dom_html.event Js.t Lwt.t

val window_scrolls :
   ?ios_html_scroll_hack:bool
  -> ?use_capture:bool
  -> (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t)
  -> unit Lwt.t
(** If [ios_html_scroll_hack] then listen on window + html + body
    instead of only window.  On iOS (8 and 9), in WkWebView and in
    Safari, some CSS properties (e.g. html{overflow:scroll;
    -webkit-overflow-scrolling: touch;}) may move the scroll event
    from window to html or to body.  For instance, with (ON) or
    without (OFF) the following CSS:
    [html{overflow:scroll;-webkit-overflow-scrolling: touch;}]
    we may observe this:

    {[
         | capture | elements receiving the scroll events
    -----+---------+-------------------------------------
    OFF  |    true | window
    -----+---------+-------------------------------------
    OFF  |   false | window
    -----+---------+-------------------------------------
    ON   |    true | window + html + body
    -----+---------+-------------------------------------
    ON   |   false | body
    -----------------------------------------------------
    ]}

    (Also, note that pure JavaScript "onscroll" attribute might be
    broken when ON.)  It's useful to listen on html even if it's only
    relevant when ON + capture=true, because we probably want, when
    capture=true, to capture the event as early as possible. *)

val click_outside :
   ?use_capture:bool
  -> ?inside:Dom_html.element Js.t
  -> #Dom_html.element Js.t
  -> Dom_html.mouseEvent Js.t Lwt.t
(** [click_outside e] returns when user clicks outside element [e].
    Will only catch clicks inside the element given as optional
    parameter [?inside] (default is [Dom_html.document##.body]). *)

[%%shared.start]

module List : sig
  val iteri2 : (int -> 'a -> 'b -> unit) -> 'a list -> 'b list -> unit
end
