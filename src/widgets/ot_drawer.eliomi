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

[%%shared.start]
(** Drawer menu for mobile and Web applications *)

(** Build a drawer menu on the left or right of the screen.
    Returns the DOM element, and functions to open and close the menu.
    It is also possible to open or close the menu by clicking on a button,
    and to swipe the menu to close it.
    If [ios_scroll_pos_fix] is true (default) each time after the drawer is
    opened, the scroll position of the document body is set to the value it
    had just before opening the drawer. This is a workaround for a bug in iOS
    mobile where if one prevents document scrolling by CSS (requires on iOS the
    CSS code: html.dr-drawer-open, html.dr-drawer-open>body {overflow: hidden})
    the document is scrolled to the top.
*)
val drawer :
  ?a:[< Html_types.div_attrib] Eliom_content.Html.attrib list ->
  ?position:[ `Left | `Right ] ->
  ?ios_scroll_pos_fix:bool ->
  [< Html_types.div_content] Eliom_content.Html.elt list ->
  [> `Div ] Eliom_content.Html.elt *
  (unit -> unit) Eliom_client_value.t *
  (unit -> unit) Eliom_client_value.t
