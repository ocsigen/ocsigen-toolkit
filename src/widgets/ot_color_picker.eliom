(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Enguerrand Decorne
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

[%%client
  type t = { hue_canvas : Dom_html.canvasElement Js.t;
             hue_cover : Dom_html.canvasElement Js.t;
             sv_canvas : Dom_html.canvasElement Js.t;
             sv_cover : Dom_html.canvasElement Js.t;
             width : int;
             mutable rgb : int * int * int }

  let set_point rgbdata x y w (r, g, b) =
    let line_offset = (int_of_float y) * w in
    let offset = ((int_of_float x) + line_offset) * 4 in
    Dom_html.pixel_set rgbdata (offset + 0) (int_of_float r);
    Dom_html.pixel_set rgbdata (offset + 1) (int_of_float g);
    Dom_html.pixel_set rgbdata (offset + 2) (int_of_float b);
    Dom_html.pixel_set rgbdata (offset + 3) 255

  let hsv_to_rgb h s v =
    let c = v *. s in
    let h1 = h /. 60. in
    let x = c *. (1. -. (abs_float ((mod_float h1 2.) -. 1.))) in
    let m = v -. c in
    let r, g, b =
      match h1 with
      | _ when h1 < 1. -> c,  x,  0.
      | _ when h1 < 2. -> x,  c,  0.
      | _ when h1 < 3. -> 0., c,  x
      | _ when h1 < 4. -> 0., x,  c
      | _ when h1 < 5. -> x,  0., c
      | _ when h1 <= 6. -> c,  0., x
      | _ -> 0., 0., 0.  in
    255. *. (r +. m),
    255. *. (g +. m),
    255. *. (b +. m)

  let get_ctx canvas = canvas##(getContext (Dom_html._2d_))

  let draw_hue_cover colorp x =
    let ctx = get_ctx colorp.hue_cover in
    colorp.hue_cover##.width := 360;
    ctx##.strokeStyle := Js.string "rgba(255, 255, 255, 192)";
    ctx##(strokeRect x (0.) (1.) (20.))

  let draw_sv_cover colorp x y =
    let ctx = get_ctx colorp.sv_cover in
    let pi = 4.0 *. atan 1.0 in
    colorp.sv_cover##.width := colorp.width;
    ctx##.strokeStyle := Js.string "rgba(255, 255, 255, 192)";
    ctx##beginPath;
    ctx##(arc x y (5.) (0.) ((2. *. pi)) (Js._false));
    ctx##stroke

  let draw_hue ctx width =
    let image = ctx##(createImageData (360) (20)) in
    let rgbdata = image##.data in
    let w = 360. in
    let inc = 360. /. 360. in
    let rec aux i =
        if i >= w then () else
          begin
            let rgb = hsv_to_rgb i 1. 1. in
            for y=0 to 20 do
              set_point rgbdata i (float_of_int y) 360 rgb;
            done;
            aux (i +. inc)
          end
    in aux 0.;
    ctx##(putImageData image (0.)  (0.))

  let draw_sv ctx hue x y (size : int) =
    let size' = float_of_int size in
    let image = ctx##(createImageData size size) in
    let rgbdata = image##.data in
    let cur_inc i = (1. /. size') *. i in
    let rec inner_aux s v =
      if s >= size' then () else
        begin
          let rgb = hsv_to_rgb hue (cur_inc v) (cur_inc s) in
          set_point rgbdata (x +. s) (y +. v) size rgb;
          inner_aux (s +. 1.) v
        end
    in let rec aux v =
      if v >= size' then () else
        begin
          inner_aux 0. v;
          aux (v +. 1.)
        end in aux 0.;
    ctx##(putImageData image (0.)  (0.))

  let init_handler colorp =
    let get_rgb pixel =
      let r = Dom_html.pixel_get pixel 0 in
      let g = Dom_html.pixel_get pixel 1 in
      let b = Dom_html.pixel_get pixel 2 in
      r, g, b
    in
    let get_coord ev canvas =
      let x, y = Dom_html.elementClientPosition canvas in
      ev##.clientX - x,
      ev##.clientY - y
    in
    Lwt_js_events.async
    (fun () ->
       Lwt_js_events.clicks colorp.sv_cover (fun ev _ ->
           let x, y = get_coord ev colorp.sv_canvas in
           let x', y' = float_of_int x, float_of_int y in
           let ctx = get_ctx colorp.sv_canvas in
           let rgbdata = ctx##(getImageData x' y' (1.) (1.))##.data in
           let r, g, b = get_rgb rgbdata in
           colorp.rgb <- r, g, b;
           draw_sv_cover colorp x' y';
           Lwt.return ()
        ));
    Lwt_js_events.async
     (fun () ->
       Lwt_js_events.clicks colorp.hue_cover (fun ev _ ->
           let x, y = get_coord ev colorp.hue_canvas in
           let x', y' = float_of_int x, float_of_int y in
           let ctx_sv = get_ctx colorp.sv_canvas in
           draw_sv ctx_sv x' 0. 0. colorp.width;
           let ctx_hue = get_ctx colorp.hue_canvas in
           let rgbdata = ctx_hue##(getImageData x' y' (1.) (1.))##.data in
           let r, g, b = get_rgb rgbdata in
           colorp.rgb <- r, g, b;
           draw_hue_cover colorp x';
           let cwidth' = float_of_int colorp.width in
           draw_sv_cover colorp cwidth' cwidth';
           Lwt.return ()
        ))

  let append_at elt colorp =
    let div = Dom_html.createDiv Dom_html.document in
    let div_hue = Dom_html.createDiv Dom_html.document in
    let div_sv = Dom_html.createDiv Dom_html.document in
    div_hue##.className := Js.string "ojw_colorpicker_sv";
    div_sv##.className := Js.string "ojw_colorpicker_hue";
    div##.className := Js.string "ojw_colorpicker";
    colorp.sv_canvas##.style##.position := Js.string "absolute";
    colorp.sv_canvas##.style##.zIndex := Js.string "-1";
    colorp.hue_canvas##.style##.position := Js.string "absolute";
    colorp.hue_canvas##.style##.zIndex := Js.string "-1";
    Dom.appendChild elt div;
    Dom.appendChild div div_hue;
    Dom.appendChild div div_sv;
    Dom.appendChild div_hue colorp.hue_canvas;
    Dom.appendChild div_hue colorp.hue_cover;
    Dom.appendChild div_sv colorp.sv_canvas;
    Dom.appendChild div_sv colorp.sv_cover

  let get_rgb colorp =
    colorp.rgb

  let create ?(width = 100) _ =
    let hue = Dom_html.createCanvas Dom_html.document in
    let sv = Dom_html.createCanvas Dom_html.document in
    let hue_cover = Dom_html.createCanvas Dom_html.document in
    let sv_cover = Dom_html.createCanvas Dom_html.document in
    let color = 0, 0, 0 in
    hue##.width := 360;
    hue_cover##.width := 360;
    sv##.width := width;
    sv_cover##.width := width;
    hue##.height := 20;
    hue_cover##.height := 20;
    sv##.height := width;
    sv_cover##.height := width;
    draw_hue (get_ctx hue) width;
    draw_sv (get_ctx sv) 0. 0. 0. width;
    {hue_canvas = hue; hue_cover = hue_cover; sv_canvas = sv;
     sv_cover = sv_cover; width = width; rgb = color }
]
