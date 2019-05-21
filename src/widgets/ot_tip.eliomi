(* Ocsigen Toolkit
 * http://www.ocsigen.org/ocsigen-toolkit
 *
 * Copyright (C) 2017
 *      Julien Sagot
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

(** {2 Tip widget} *)

(** This module implement a [display] function which actually display a tip
    (i.e. a box over the page content).

    It appends an element (which is called here "the filter") to body. The filter is
    meant take the whole page space, in order to capture clicks to close the tip.
    The filter also contains another element, called the menu which is the container
    for content to be displayed.

    [filter_a]: filter attributes default is [ a_class ["ot-drp-filter"] ]
    and will be overriden if you provide this argument.

    [menu_a]: menu attributes default is [ [ a_class ["ot-drp-menu"] ] ]
    and will be overriden if you provide this argument.

    [side]: specify how the tip whould be positioned with respect to
    the [origin] element. By default, the tip is centered; if it would
    not fit on screen, its right hand side or left hand side is aligned
    with the middle of the [origin] element. When side is [`Left] or
    [`Right], the tip and the [origin] element are aligned on the
    right (resp. the left).

    [origin] is the element from which the tip is supposed to pop out.

    [onopen filter menu side] is called after the filter is append to body.

    [onclose filter menu side] is called after the tip has been closed.

    [content] is the function generating the main content. It takes the function
    to close the tip as parameter

 *)

val display
  : ?container_a:[< Html_types.div_attrib > `Class] Eliom_content.Html.attrib list
  -> ?filter_a:[< Html_types.div_attrib > `Class `OnClick]
    Eliom_content.Html.attrib list
  -> ?side:[ `Left | `Right | `Center ]
  -> origin : Dom_html.element Js.t
  -> ?onopen:([> Html_types.div ] Eliom_content.Html.elt
              -> [> Html_types.div ] Eliom_content.Html.elt
              -> unit)
  -> ?onclose:([> Html_types.div ] Eliom_content.Html.elt
              -> [> Html_types.div ] Eliom_content.Html.elt
              -> unit)
  -> content: ((unit -> unit)
               -> [< Html_types.div_content_fun > `Div]
                 Eliom_content.Html.elt list)
  -> unit
  -> ( [> Html_types.div ] Eliom_content.Html.elt
       * (unit -> unit) )
