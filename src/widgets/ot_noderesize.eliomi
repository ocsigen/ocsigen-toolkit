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

(* This is a redo of [ResizeSensor.js] which is MIT licensed, with few
   patches
   https://github.com/marcj/css-element-queries/blob/master/src/ResizeSensor.js
*)

[%%client.start]

(** {2 Get an event when an element's size changes}

    {3 Known issues}

    This only work with elements in the DOM (maybe that the element
    has to be displayd, need to check this).  In case of a content
    loaded dynamically with js, watch a parent already thereif
    possible or use a [onnodeready] event to attach [noderesize]
    listener.

    Also, if the element is removed, then re-inserted in the DOM, sensor
    will not work anymore.

    If the element to be watched is not positionned, a [position:
    relative] will be applied.

    {3 Example}

    {[Lwt.async (fun () ->
        let div' = (To_dom.of_element div) in
        let%lwt () = Nodeready.nodeready container' in
        Ot_noderesize.noderesize (Noderesize.init div) (fun () ->
          Firebug.console##log (Js.string "Resized") ) )]} *)

type resize_sensor

val attach : #Dom_html.element Js.t -> resize_sensor

val noderesize : resize_sensor -> (unit -> unit) -> unit

(** Same as noderesize, but use static values for sensor in order to avoid
    reading parent watched element's size for each sensor reset. *)
val noderesize_opt : resize_sensor -> (unit -> unit) -> unit

val detach : resize_sensor -> unit
