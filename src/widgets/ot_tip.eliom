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
  let m = To_dom.of_element container in
  let bb = origin##getBoundingClientRect in
  let w = bb##.right -. bb##.left in
  let top = bb##.top in
  let bottom =
    float_of_int Dom_html.document##.documentElement##.clientHeight
    -. bb##.bottom
  in
  let left = bb##.left in
  let right =
    float_of_int Dom_html.document##.documentElement##.clientWidth -. bb##.right
  in
  let print_px x = Js.string (Printf.sprintf "%gpx" x) in
  m##.style##.minWidth := print_px w;
  (if top < bottom
  then (
    let top = print_px bb##.bottom in
    m##.style##.top := top;
    m##.classList##add (Js.string "ot-tip-top"))
  else
    let bottom =
      print_px
        (float_of_int Dom_html.document##.documentElement##.clientHeight
        -. bb##.top)
    in
    m##.style##.bottom := bottom;
    m##.classList##add (Js.string "ot-tip-bottom"));
  (match side with
  | `Left ->
      let right =
        float_of_int Dom_html.document##.documentElement##.clientWidth
        -. bb##.right
      in
      m##.style##.right := print_px right;
      Manip.Class.add container "ot-tip-left"
  | `Right ->
      let left = bb##.left in
      m##.style##.left := print_px left;
      Manip.Class.add container "ot-tip-right"
  | `Center ->
      if right < left
      then (
        let right =
          float_of_int Dom_html.document##.documentElement##.clientWidth
          -. ((bb##.right +. bb##.left) /. 2.)
        in
        m##.style##.right := print_px right;
        Lwt.async @@ fun () ->
        let%lwt () = Ot_nodeready.nodeready m in
        let off = float (m##.offsetWidth / 2) in
        if off <= right -. 1.
        then (
          m##.style##.right := print_px (right -. off);
          Manip.Class.add container "ot-tip-center")
        else Manip.Class.add container "ot-tip-left";
        Lwt.return_unit)
      else
        let left = (bb##.right +. bb##.left) /. 2. in
        m##.style##.left := print_px left;
        Lwt.async @@ fun () ->
        let%lwt () = Ot_nodeready.nodeready m in
        let off = float (m##.offsetWidth / 2) in
        if off <= left -. 1.
        then (
          m##.style##.left := print_px (left -. off);
          Manip.Class.add container "ot-tip-center")
        else Manip.Class.add container "ot-tip-right";
        Lwt.return_unit);
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
