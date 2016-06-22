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

(* NOTE: TODO: rework this doc: be careful when using the following three functions.
 * They may start before the new document is displayed (and thus the new window
 * is there) and therefore may be attached to the window that is about to be
 * replaced. In most use-cases you should have a line as follows before:
 * let%lwt () = Ot_nodeready.nodeready @@ To_dom.of_element some_elt in
 * TODO: document the ios_html_scroll_hack workaround
 *)
val window_scroll : ?use_capture:bool -> unit -> Dom_html.event Js.t Lwt.t
val window_scrolls : ?ios_html_scroll_hack:bool -> ?use_capture:bool ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

(** [click_outside e] returns when user clicks outside element [e]. *)
val click_outside :
  ?use_capture:bool -> #Dom_html.element Js.t -> Dom_html.mouseEvent Js.t Lwt.t


[%%shared.start]
module List : sig
  val iteri2 : (int -> 'a -> 'b -> unit) -> 'a list -> 'b list -> unit
end
