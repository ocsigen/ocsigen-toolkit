(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015 BeSport, Julien Sagot
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

[%%client open Eliom_content.Html ]
[%%client open Eliom_content.Html.F ]

[%%client type resize_sensor =
            { watched : Dom_html.element Js.t
            ; grow : Dom_html.element Js.t
            ; mutable grow_listener_id : Dom.event_listener_id option
            ; grow_child : Dom_html.element Js.t
            ; shrink : Dom_html.element Js.t
            ; mutable shrink_listener_id : Dom.event_listener_id option
            ; sensor : Dom_html.element Js.t } ]

let%client attach watched =
  let style = "display:block; position: absolute; \
               left: 0; top: 0; right: 0; bottom: 0; \
               overflow: hidden; z-index: -1000; visibility: hidden;" in
  let style_child = "position: absolute; left: 0; top: 0;" in
  let grow_child = D.div ~a:[ a_style style_child ] [] in
  let grow =
    D.div ~a:[ a_class ["resize-sensor-grow"] ; a_style style ] [ grow_child ]
  in
  let shrink =
    D.div ~a:[ a_class ["resize-sensor-shrink"] ; a_style style ]
      [ div ~a:[ a_style (style_child ^ " width: 200%; height: 200%;") ] [] ] in
  let sensor = D.div ~a:[ a_class ["resize-sensor"] ; a_style style ]
      [ grow ; shrink ] in
  let grow_child = To_dom.of_element grow_child in
  let grow = To_dom.of_element grow in
  let shrink = To_dom.of_element shrink in
  let sensor = To_dom.of_element sensor in
  if (Dom_html.window##getComputedStyle watched)##.position = Js.string "static"
  then watched##.style##.position := Js.string "relative" ;
  Dom.appendChild watched sensor ;
  { watched = (watched :> Dom_html.element Js.t)
  ; grow ; grow_child ; shrink ; sensor
  ; grow_listener_id = None ; shrink_listener_id = None }

let%client detach
    { watched ; sensor ; shrink_listener_id ; grow_listener_id ; _ } =
  Dom.removeChild watched sensor ;
  (match grow_listener_id with Some x -> Dom.removeEventListener x | _ -> ()) ;
  (match shrink_listener_id with Some x -> Dom.removeEventListener x | _ -> ())

let%client reset { grow ; grow_child ; shrink ; _ } =
  shrink##.scrollLeft := shrink##.scrollWidth ;
  shrink##.scrollTop := shrink##.scrollHeight ;
  grow_child##.style##.width :=
    Js.string ((string_of_int (grow##.offsetWidth + 1)) ^ "px") ;
  grow_child##.style##.height :=
    Js.string ((string_of_int (grow##.offsetHeight + 1)) ^ "px") ;
  grow##.scrollLeft := grow##.scrollWidth ;
  grow##.scrollTop := grow##.scrollHeight

let%client noderesize sensor f =
  let bind element =
    let w = ref element##.offsetWidth in
    let h = ref element##.offsetHeight in
    let throttle = ref false in
    Dom.addEventListener element (Dom_html.Event.scroll)
      (Dom.handler (fun _ ->
         if not !throttle then begin
           throttle := true ;
           ignore @@ Dom_html.window##requestAnimationFrame
             (Js.wrap_callback @@ fun _ ->
              let w' = element##.offsetWidth in
              let h' = element##.offsetHeight in
              if w' <> !w || h' <> !h then f () ;
              w := w' ;
              h := h' ;
              reset sensor ;
              throttle := false) end ;
         Js.bool true) )
      (Js.bool false) in
  reset sensor ;
  sensor.grow_listener_id <- Some (bind sensor.grow) ;
  sensor.shrink_listener_id <- Some (bind sensor.shrink)
