(* Ocsigen-toolkit
 * http://www.ocsigen.org/ocsigen-toolkit
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Charly Chevalier
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
(* size and orientation *)
type orientation = Portrait | Landscape

let get_screen_size () =
  let scr = Dom_html.window##.screen in
  scr##.width, scr##.height

let get_screen_orientation () =
  let width, height = get_screen_size () in
  if (width <= height) then Portrait else Landscape

let get_size dom_html =
  dom_html##.clientWidth, dom_html##.clientHeight

let get_document_size () =
  get_size Dom_html.document##.documentElement

(* No: this must be recomputed every time,
   otherwise it won't work after a change page
   -- Vincent
let page = Dom_html.document##documentElement
*)

let wh, set_wh =
  let page = Dom_html.document##.documentElement in
  React.S.create (page##.clientWidth, page##.clientHeight)

let update_width_height () =
  let page = Dom_html.document##.documentElement in
  let w = page##.clientWidth in
  let h = page##.clientHeight in
  set_wh (w, h)

let width_height, width, height =
  (* TODO: MutationObserver? *)
  Lwt_js_events.(async @@ fun () -> onresizes @@ fun _ _ ->
    Lwt.return @@ update_width_height ());
  let w = React.S.l1 fst wh in
  let h = React.S.l1 snd wh in
  (* Make sure the signals are not destroyed indirectly
     by a call to React.S.stop *)
  ignore (React.S.map (fun _ -> ()) w);
  ignore (React.S.map (fun _ -> ()) h);
  (wh, w, h)

let set_adaptative_width elt f =
  (*VVV Warning: it works only because we do not have weak pointers
    on client side, thus the signal is not garbage collected.
    If Weak is implemented on client side, we must keep a pointer
    on this signal in the element *)
  ignore (React.S.map
            (fun w -> elt##.style##.width :=
              Js.string (string_of_int (f w)^"px")) height)

let set_adaptative_height elt f =
  (*VVV see above *)
  ignore
    (React.S.map
       (fun w -> elt##.style##.height :=
         Js.string (string_of_int (f w)^"px")) height)

let of_opt elt =
  Js.Opt.case elt (fun () -> failwith "of_opt") (fun x -> x)

let height_to_bottom offset elt =
  let page = Dom_html.document##.documentElement in
  let h = page##.clientHeight in
  try
    let top = Js.to_float (of_opt (elt##getClientRects##(item (0))))##.top in
    h - int_of_float top - offset
  with Failure _ -> h - offset

let client_top ?(with_border = true) elt =
  Js.to_float elt##getBoundingClientRect##.top -.
  if with_border then Ot_style.marginTop elt else 0.0
let client_bottom ?(with_border = true) elt =
  Js.to_float elt##getBoundingClientRect##.bottom -.
  if with_border then Ot_style.marginBottom elt else 0.0
let client_left ?(with_border = true) elt =
  Js.to_float elt##getBoundingClientRect##.left -.
  if with_border then Ot_style.marginLeft elt else 0.0
let client_right ?(with_border = true) elt =
  Js.to_float elt##getBoundingClientRect##.right -.
  if with_border then Ot_style.marginRight elt else 0.0
let client_height ?(with_border = true) elt =
  client_bottom ~with_border elt -. client_top ~with_border elt
let client_width ?(with_border = true) elt =
  client_right ~with_border elt -. client_left ~with_border elt

let client_page_top elt =
  elt##getBoundingClientRect##.top -. (*TODO: use the above functions*)
  Dom_html.document##.body##getBoundingClientRect##.top

let client_page_left elt =
  elt##getBoundingClientRect##.left -.
  Dom_html.document##.body##getBoundingClientRect##.left

let pageYOffset () = (* absolute vertical scroll position *)
  let get_clientHeight () =
    Dom_html.document##.documentElement##.clientHeight
  in
  (* on some browsers innerHeight is not available -> fall back to clientHeight *)
  let get_innerHeight () =
    try (Js.Unsafe.coerce Dom_html.window)##.innerHeight
    with _ -> get_clientHeight ()
  in
  max 0 @@ (* overscroll at the top *)
  min      (* overscroll at the bottom *)
    (Dom_html.document##.documentElement##.scrollHeight - get_innerHeight ())
    ((Js.Unsafe.coerce Dom_html.window)##.pageYOffset)
