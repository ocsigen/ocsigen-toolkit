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

open%client Eliom_content.Html
open%client Eliom_content.Html.F

let%client display
    ?(container_a = [ a_class ["ot-tip-container"] ])
    ?(filter_a = [ a_class ["ot-tip-filter"] ])
    ?(side : [ `Center | `Left | `Right ] option)
    ~(origin : Dom_html.element Js.t)
    ?(onopen = fun _ _ _ () -> ())
    ?(onclose = fun _ _ _ () -> ())
    ~(content : (unit -> unit) ->
      [< Html_types.div_content_fun > `Div] Eliom_content.Html.elt list) () =
  let close = ref @@ fun () -> () in
  let b = origin in
  let container =
    D.div ~a:container_a @@
    div ~a:[a_class ["ot-tip-src"]] []
    :: content (fun () -> !close ())
  in
  let m = To_dom.of_element container in
  let mb = b##getBoundingClientRect in
  let w = mb##.right -. mb##.left in
  let bb = b##getBoundingClientRect in
  let top = int_of_float bb##.top in
  let bottom = Dom_html.document##.documentElement##.clientHeight
               - int_of_float bb##.bottom in
  let left = int_of_float bb##.left in
  let right = Dom_html.document##.documentElement##.clientWidth
              - int_of_float bb##.right in
  let print_px x = Js.string (Printf.sprintf "%gpx" x) in
  m##.style##.minWidth := print_px w ;
  if top < bottom
  then ( let top = print_px bb##.bottom in
         m##.style##.top := top
       ; m##.classList##add (Js.string "ot-tip-top") )
  else (let bottom =
          print_px
            (float_of_int Dom_html.document##.documentElement##.clientHeight
             -. bb##.top) in
        m##.style##.bottom := bottom
      ; m##.classList##add (Js.string "ot-tip-bottom") ) ;
  let side =
    if side = Some `Center || (side = None && right = left)
    then (Lwt.async (fun () ->
      let%lwt () = Ot_nodeready.nodeready m in
      let _ = Dom_html.window##getComputedStyle m in (* Force layout *)
      m##.style##.left :=
        print_px (((bb##.right +. bb##.left) /. 2.)
                  -. (float_of_int m##.offsetWidth) /. 2.)
    ; Lwt.return_unit)
         ; m##.classList##add (Js.string "ot-tip-center")
         ; `Center )
    else if side = Some `Left || (side = None && right < left)
    then (let right =
            print_px
              (float_of_int Dom_html.document##.documentElement##.clientWidth
               -. bb##.right) in
          m##.style##.right := right
        ; m##.classList##add (Js.string "ot-tip-left")
        ; `Left)
    else (* if side = Some `Right || (side = None && right > left) *)
      ( m##.style##.left := print_px bb##.left
      ; m##.classList##add (Js.string "ot-tip-right")
      ; `Right )
  in
  let filter =
    D.div ~a:(a_onclick (fun _ -> !close ()) :: filter_a) [ container ]
  in
  let scroll_handler =
    Dom.addEventListener Dom_html.document
      Dom_html.Event.scroll
      (Dom_html.handler (fun _ -> !close () ; Js._true))
      Js._false
  in
  close := (fun () ->
    Dom.removeEventListener scroll_handler;
    Manip.removeSelf filter;
    onclose filter container side () ;
  );
  Manip.appendToBody filter ;
  onopen filter container side () ;
  (filter, !close)
