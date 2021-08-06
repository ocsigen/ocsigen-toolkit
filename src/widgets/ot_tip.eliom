[%%client
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

open Js_of_ocaml]

[%%client open Eliom_content.Html]
[%%client open Eliom_content.Html.F]

let%client display ?(container_a = [a_class ["ot-tip-container"]])
    ?(filter_a = [a_class ["ot-tip-filter"]])
    ?(side : [`Center | `Left | `Right] = `Center)
    ~(origin : Dom_html.element Js.t) ?(onopen = fun _ _ -> ())
    ?(onclose = fun _ _ -> ())
    ~(content :
       (unit -> unit)
       -> [< Html_types.div_content_fun > `Div] Eliom_content.Html.elt list) ()
  =
  let close = ref @@ fun () -> () in
  let container =
    D.div ~a:container_a
    @@ (div ~a:[a_class ["ot-tip-src"]] [] :: content (fun () -> !close ()))
  in
  let container_elt = To_dom.of_element container in
  let d_height = float Dom_html.document##.documentElement##.clientHeight in
  let d_width = float Dom_html.document##.documentElement##.clientWidth in
  let o_bounds = origin##getBoundingClientRect in
  let o_left = o_bounds##.left in
  let o_right = o_bounds##.right in
  let o_to_right = d_width -. o_right in
  let o_top = o_bounds##.top in
  let o_to_top = d_height -. o_top in
  let o_bottom = o_bounds##.bottom in
  let o_to_bottom = d_height -. o_bottom in
  let o_width = o_right -. o_left in
  let o_center_to_left = (o_right +. o_left) /. 2. in
  let o_center_to_right = d_width -. o_center_to_left in
  let container_ready = Ot_nodeready.nodeready container_elt in
  let when_container_ready get_from_container use_it =
    Lwt.(async @@ fun () -> container_ready >|= get_from_container >|= use_it)
  in
  let get_c_height () = float container_elt##.offsetHeight in
  let get_half_c_width () = float (container_elt##.offsetWidth / 2) in
  let c_style = container_elt##.style in
  let print_px x = Js.string (Printf.sprintf "%gpx" x) in
  let c_add_class class_ = Manip.Class.add container class_ in
  c_style##.minWidth := print_px o_width;
  let put_on_top () =
    c_style##.top := print_px 0.;
    c_add_class "ot-tip-bottom"
  in
  let put_c_below_o () =
    c_style##.top := print_px o_bottom;
    c_add_class "ot-tip-top"
  in
  let put_c_above_o () =
    c_style##.bottom := print_px o_to_top;
    c_add_class "ot-tip-bottom"
  in
  if o_top < o_to_bottom then put_c_below_o () else put_c_above_o ();
  (match side with
  | `Left ->
      c_style##.right := print_px o_to_right;
      c_add_class "ot-tip-left"
  | `Right ->
      c_style##.left := print_px o_left;
      c_add_class "ot-tip-right"
  | `Center ->
      if o_to_right < o_left
      then (
        c_style##.right := print_px o_center_to_right;
        when_container_ready get_half_c_width (fun half_c_width ->
            if half_c_width <= o_center_to_right -. 1.
            then (
              c_style##.right := print_px (o_center_to_right -. half_c_width);
              c_add_class "ot-tip-center")
            else c_add_class "ot-tip-left"))
      else (
        c_style##.left := print_px o_center_to_left;
        when_container_ready get_half_c_width (fun half_c_width ->
            if half_c_width <= o_center_to_left -. 1.
            then (
              c_style##.left := print_px (o_center_to_left -. half_c_width);
              c_add_class "ot-tip-center")
            else c_add_class "ot-tip-right")));
  let filter =
    D.div ~a:(a_onclick (fun _ -> !close ()) :: filter_a) [container]
  in
  let scroll_handler =
    Dom.addEventListener Dom_html.document Dom_html.Event.scroll
      (Dom_html.handler (fun _ -> !close (); Js._true))
      Js._false
  in
  (close :=
     fun () ->
       Dom.removeEventListener scroll_handler;
       Manip.removeSelf filter;
       onclose filter container);
  Manip.appendToBody filter; onopen filter container; filter, !close
