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

    Function returns the element, the current position (as a react signal),
    and the number of visible elements. It is more than 1 if element width
    ([div.car2]) is set, in CSS, to a value smaller than carousel width
    (for example 50% or 33.33%).
    The width of all elements is supposed to be equal,
    and the size of carousel must be a multiple of the size of element.
*)
val make :
  ?a: [< Html5_types.div_attrib > `Class ] Eliom_content.Html5.D.attrib list ->
  ?vertical:bool ->
  ?position:int ->
  ?update: [`Goto of int | `Next | `Prev ] React.event Eliom_client_value.t ->
  ?disabled: bool Eliom_shared.React.S.t ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt list ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  int Eliom_shared.React.S.t *
  int Eliom_shared.React.S.t

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
 *)
val bullets :
  ?a:[< Html5_types.ul_attrib > `Class ] Eliom_content.Html5.F.attrib list ->
  ?attributes:[< Html5_types.li_attrib > `Class `OnClick ]
    Eliom_content.Html5.F.attrib list list ->
  change: ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t ->
  pos:int Eliom_shared.React.S.t ->
  length:int ->
  ?size:int Eliom_shared.React.S.t ->
  unit -> [> Html5_types.ul ] Eliom_content.Html5.F.elt

(** Menu for carousel. Current page has class ["active"].
    [pos] is a signal corresponding to current position.
    [change] is a function to change position of carousel.
    This is usually the function to trigger the event given as
    parameter to [make].
    Optional parameter [size] is the number of elements visible at the same time
    in the carousel (return by function [make]).
    The last argument is the list of titles, for each carousel page,
    that will be included in [<li>] tags.
 *)
val ribbon :
  ?a:[< Html5_types.ul_attrib > `Class `OnClick ]
    Eliom_content.Html5.F.attrib list ->
  change: ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t ->
  pos:int Eliom_shared.React.S.t ->
  ?size:int Eliom_shared.React.S.t ->
  [< Html5_types.li_content_fun ] Eliom_content.Html5.F.elt list list ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt

val previous :
  ?a:[< Html5_types.button_attrib ] Eliom_content.Html5.attrib list
  -> change: ([> `Prev ] -> unit) Eliom_client_value.t
  -> pos:int Eliom_shared.React.S.t
  -> Html5_types.button_content Eliom_content.Html5.elt list
  -> [> Html5_types.button ] Eliom_content.Html5.elt

val next :
    ?a:[< Html5_types.button_attrib ] Eliom_content.Html5.attrib list
  -> change: ([> `Next ] -> unit) Eliom_client_value.t
  -> pos:int Eliom_shared.React.S.t
  -> size:int Eliom_shared.React.S.t
  -> length:int
  -> Html5_types.button_content Eliom_content.Html5.elt list
  -> [> Html5_types.button ] Eliom_content.Html5.elt

(* (\** Menu + prev/next buttons *\) *)
(* val nav : *)
(*   ?a:[< Html5_types.ul_attrib > `Class `OnClick ] *)
(*     Eliom_content.Html5.F.attrib list -> *)
(*   change: ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t -> *)
(*   pos:int Eliom_shared.React.S.t -> *)
(*   ?size:int Eliom_shared.React.S.t -> *)
(*   [< Html5_types.li_content_fun ] Eliom_content.Html5.F.elt list list -> *)
(*   [> Html5_types.div ] Eliom_content.Html5.F.elt *)


[%%client.start]
(**  Make arrow keys cause event change.
     Returns a thread that never stops until you call [Lwt.cancel] on it. *)
val bind_arrow_keys :
  ?use_capture:bool ->
  ?vertical:bool ->
  change: ([> `Goto of int | `Next | `Prev ] -> unit) ->
  #Dom_html.eventTarget Js.t ->
  unit Lwt.t
