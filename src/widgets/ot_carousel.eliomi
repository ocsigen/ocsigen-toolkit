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

(**

   Carousel.

   This is a widgets containing blocks. One or several blocks are displayed
   at a time, depending on the size of the carousel.
   User can display the next/previous items by pressing buttons,
   by swiping on touch screens, or by pressing arrow keys.

   Carousel can be horizontal or vertical.

   This module also defines two other widgets related to the carousel:
   - bullets displays the current position in the carousel (as a set of bullets),
   - ribbon display a swipable horizontal menu to navigate the carousel.

   You can define all these widgets in client or server OCaml side programs.

*)


[%%shared.start]
(**
  Creates a carousel from the elements of a list.
    [?position] is the initial position (default 0).
    [?update] is a react event you can use to command the carousel from outside.
    [?disabled] is always [false] by default. When [true], it is not possible
    to change carousel position.
    [?transition_duration] must match the values in CSS (default: 0.6s).

    Use optional parameter [?full_height]
    if you don't want scroll bars in carousel
    pages. Carousel will expand as necessary in the page, and swiping
    to another page will automatically update the scroll position of the
    whole page. Use this for example if you want a page with several tabs.
    [`No_header] means that the top of the screen will be used for the
    default position. Use [`Header f] if you want to add some extra space
    for a header (for example a tabbar). [f] is the function which returns
    the height of this header.

    Function returns:
    - the element,
    - the current position (as a react signal),
    - the number of visible elements. It is more than 1 if element width
    ([div.car2]) is set, in CSS, to a value smaller than carousel width
    (for example 50% or 33.33%).
    The width of all elements is supposed to be equal,
    and the size of carousel must be a multiple of the size of element.
    - the current swipe position. Value -1.0 corresponds to previous page,
    and +1.0 to next page.
*)
val make :
  ?a: [< Html_types.div_attrib ] Eliom_content.Html.attrib list ->
  ?vertical:bool ->
  ?position:int ->
  ?transition_duration:float ->
  ?update: [ `Goto of int | `Next | `Prev ] React.event Eliom_client_value.t ->
  ?disabled: bool Eliom_shared.React.S.t ->
  ?full_height:[ `No | `No_header
               | `Header of (unit -> int) Eliom_client_value.t ] ->
  [< Html_types.div_content ] Eliom_content.Html.elt list ->
  [> `Div ] Eliom_content.Html.elt *
  int Eliom_shared.React.S.t *
  int Eliom_shared.React.S.t *
  float React.S.t Eliom_client_value.t

(** List of bullets for carousel. Current page has class ["active"].
    [pos] is a signal corresponding to current position.
    [change] is a function to change position of carousel.
    This is usually the function to trigger the event given as
    parameter to [make].
    [length] must be exactly the number of elements in the carousel.
    Optional parameter [attributes] makes possible to give an HTML attribute
    to each bullet (for example a reactive class).
    Optional parameter [size] is the number of elements visible at the same time
    in the carousel (return by function [make]).
    Optional parameter [content] makes it possible to fill the bullets with Html
    elements.
 *)
val bullets :
  ?a:[< Html_types.ul_attrib ] Eliom_content.Html.attrib list ->
  ?attributes:
    [< Html_types.li_attrib ] Eliom_content.Html.attrib list list ->
  change: ([`Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t ->
  pos:int Eliom_shared.React.S.t ->
  length:int ->
  ?size:int Eliom_shared.React.S.t ->
  ?content: [< Html_types.li_content_fun ] Eliom_content.Html.elt list list ->
  unit -> [> `Ul ] Eliom_content.Html.elt

(** Menu (or tabs) for carousel. Current page has class ["active"].
    [pos] is a signal corresponding to current position.
    [change] is a function to change position of carousel.
    This is usually the function to trigger the event given as
    parameter to [make].
    Optional parameter [size] is the number of elements visible at the same time
    in the carousel (return by function [make]).
    Optional parameter [gap] is the default gap on the left, in pixels.
    The last argument is the list of titles, for each carousel page,
    that will be included in [<li>] tags.

    If [cursor] is present, an element is added below the ribbon
    to visualize the current position. It corresponds to the float
    signal returned by [make].
 *)
val ribbon :
  ?a:[< Html_types.ul_attrib ] Eliom_content.Html.attrib list ->
  change: ([ `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t ->
  pos:int Eliom_shared.React.S.t ->
  ?size:int Eliom_shared.React.S.t ->
  ?initial_gap:int ->
  ?transition_duration:float ->
  ?cursor:float React.S.t Eliom_client_value.t ->
  [< Html_types.li_content_fun ] Eliom_content.Html.elt list list ->
  [> `Div ] Eliom_content.Html.elt

(** Go to the previous page (or mores page if [step] is present). *)
val previous :
  ?a:[< Html_types.button_attrib ] Eliom_content.Html.attrib list
  -> change: ([> `Prev | `Goto of int ] -> unit) Eliom_client_value.t
  -> ?step:int Eliom_shared.React.S.t
  -> pos:int Eliom_shared.React.S.t
  -> Html_types.button_content Eliom_content.Html.elt list
  -> [> `Button ] Eliom_content.Html.elt

(** Go to the next page (or more pages if [step] is present). *)
val next :
    ?a:[< Html_types.button_attrib ] Eliom_content.Html.attrib list
  -> change: ([> `Next | `Goto of int ] -> unit) Eliom_client_value.t
  -> ?step:int Eliom_shared.React.S.t
  -> pos:int Eliom_shared.React.S.t
  -> size:int Eliom_shared.React.S.t
  -> length:int
  -> Html_types.button_content Eliom_content.Html.elt list
  -> [> `Button ] Eliom_content.Html.elt

(* (\** Menu + prev/next buttons *\) *)
(* val nav : *)
(*   ?a:[< Html_types.ul_attrib > `Class `OnClick ] *)
(*     Eliom_content.Html.F.attrib list -> *)
(*   change: ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t -> *)
(*   pos:int Eliom_shared.React.S.t -> *)
(*   ?size:int Eliom_shared.React.S.t -> *)
(*   [< Html_types.li_content_fun ] Eliom_content.Html.F.elt list list -> *)
(*   [> Html_types.div ] Eliom_content.Html.F.elt *)


[%%client.start]
(**  Make arrow keys cause event change.
     Returns a thread that never stops until you call [Lwt.cancel] on it. *)
val bind_arrow_keys :
  ?use_capture:bool ->
  ?vertical:bool ->
  change: ([> `Goto of int | `Next | `Prev ] -> unit) ->
  #Dom_html.eventTarget Js.t ->
  unit Lwt.t
