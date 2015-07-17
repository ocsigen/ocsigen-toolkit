(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Vasilis Papavasileiou
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

{client{

module Eliom_lib = struct
  (* copied from Eliom_csreact ; find a proper place for this *)
  include Eliom_lib
  let create_shared_value _ x = x
end

let r_node a = Eliom_content.Html5.R.node a

}}

{server{ let r_node a = Eliom_csreact.R.node a }} ;;

{shared{

open Eliom_content

open Html5.D

type polar = int * int

type cartesian = int * int

type time_receiver = (int -> int -> unit) Eliom_lib.client_value

type 'a rf = ?step:React.step -> 'a -> unit

type 'a rp = 'a React.signal * 'a rf

(* utils *)

let rec list_init ?acc:(acc = []) i f =
  if i <= 0 then
    acc
  else
    let acc = f (i - 1) :: acc in
    list_init ~acc (i - 1) f

(* http://stackoverflow.com/questions/30732709 *)

let round_lb = -.(2. ** 52.)

let round_ub = 2. ** 52.

let round x =
  if round_lb <= x && x <= round_ub then
    floor (x +. 0.49999999999999994)
  else
    x

(* polar / cartesian / hours:minutes conversions *)

(* functions perform implicit rotation, so that angle = 0 means noon /
   midnight *)

let center_x = 50

let center_y = 50

let polar_to_cartesian (r, h) =
  let r = float_of_int r
  and h = float_of_int h in
  let h = h *. 3.14159 /. 180. in
  let x = center_x + int_of_float (r *. sin h)
  and y = center_y - int_of_float (r *. cos h) in
  x, y

}} ;;

{shared{

   let pi = 3.14159

let cartesian_to_polar (x, y) =
  let x = float_of_int (x - center_x)
  and y = float_of_int (y - center_y) in
  let r = hypot x y |> round |> int_of_float
  and e =
    let e = atan2 y x *. 180. /. pi |> round |> int_of_float in
    (e + 450) mod 360
  in
  r, e

let cartesian_to_angle (x, y) =
  cartesian_to_polar (x, y) |> snd

let angle_to_hours_minutes e =
  let m = e * 2 in
  let h = m / 60
  and m = m mod 60 in
  assert (0 <= h && h <= 11);
  assert (0 <= m && m <= 59);
  h, m

let angle_to_hours e =
  let h, m = angle_to_hours_minutes e in
  let h = if m > 30 then h + 1 else h in
  if h = 12 then 0 else h

let angle_to_minutes e =
  let m = e / 6 in
  assert (0 <= m && m <= 59);
  if m = 60 then 0 else m

let cartesian_to_hours_minutes (x, y) =
  cartesian_to_polar (x, y) |> snd |> angle_to_hours_minutes

(* build SVG *)

let cartesian_of_i ?radius:(radius = 40) de i =
  let e = i * de in
  polar_to_cartesian (radius, e)

}}

{client{

   let clock_reactive_hand
       ?radius:(radius = React.S.const 31)
       e =
     let display r e =
       let a =
         let s =
           let x, y = polar_to_cartesian (r, e) in
           Printf.sprintf "M %d %d L %d %d"
             center_x center_y
             x y
         in
         [Eliom_content.Svg.D.a_class ["ot-tp-hand"];
          Eliom_content.Svg.D.a_d s]
       in
       Eliom_content.Svg.D.path ~a []
     in
     Eliom_content.Svg.R.node (React.S.l2 display radius e)

let clock_reactive_hand_circle
    ?radius:(radius = 9)
    ?at_radius:(at_radius = React.S.const 40)
    e =
  let open Eliom_content.Svg.F in
  let display r e =
    let a =
      let x, y = polar_to_cartesian (r, e) in
      [a_class ["ot-tp-hand-circle"];
       a_r (float_of_int radius, Some `Px);
       a_cx (float_of_int x, Some `Px);
       a_cy (float_of_int y, Some `Px)]
    in
    circle ~a [title (pcdata "" )]
  in
  Eliom_content.Svg.R.node (React.S.l2 display at_radius e)

}}

{shared{

   let make_clock_point
       ?zero_is_12:(zero_is_12 = false)
       extra_attributes radius de step i =
     let open Eliom_content.Svg.F in
     let x, y = cartesian_of_i ~radius de i in
     let a =
       a_class ["ot-tp-text"]
       :: a_dominant_baseline `Central
       :: a_text_anchor `Middle
       :: a_x_list [float_of_int x, Some `Px]
       :: a_y_list [float_of_int y, Some `Px]
       :: extra_attributes
     and txt =
       if i = 0 && zero_is_12 then
         "12"
       else
         string_of_int (step * i)
     in
     text ~a [pcdata txt]

let clock_svg
    ?zero_is_12
    ?extra_attributes:(extra_attributes = [])
    ?n:(n = 12)
    ?step:(step = 1)
    (e : int Eliom_csreact.SharedReact.S.t) =
  assert (n >= 0 && 360 mod n = 0);
  let de = 360 / n in
  let l =
    make_clock_point ?zero_is_12 extra_attributes 40 de step |>
    list_init n
  and h1 =
    Eliom_content.Svg.C.node {{ clock_reactive_hand %e }}
  and h2 =
    Eliom_content.Svg.C.node {{ clock_reactive_hand_circle %e }}
  in
  Eliom_content.Svg.D.g (h1 :: h2 :: l)

let clock_svg_24h
    ?extra_attributes:(extra_attributes = [])
    b h =
  let de = 30 in
  let l1 = list_init 12 (make_clock_point extra_attributes 30 de 1)
  and l2 =
    let f i = make_clock_point extra_attributes 42 de 1 (i + 12) in
    list_init 12 f
  and h1 =
    let radius =
      {int React.signal{
         let f b = if b then 24 else 36 in
         React.S.map f %b }}
    in
    {{ clock_reactive_hand ~radius:%radius %h }} |>
    Eliom_content.Svg.C.node
  and h2 =
    let at_radius =
      {int React.signal{
         let f b = if b then 30 else 42 in
         React.S.map f %b }}
    in
    {{ clock_reactive_hand_circle
         ~radius:6 ~at_radius:%at_radius %h }} |>
    Eliom_content.Svg.C.node
  in
  Eliom_content.Svg.D.g (h1 :: h2 :: l1 @ l2)

let convert_24h is_am h = if is_am then h else h + 12

}} ;;

{client{

   let (>>!) = Js.Opt.iter

let wrap_click_aux ev f =
  ev##currentTarget >>! fun a ->
  let r = a##getBoundingClientRect () in
  let ox = r##left
  and ox' = r##right
  and oy = r##top
  and oy' = r##bottom
  and x = ev##clientX
  and y = ev##clientY in
  assert (ox' > ox);
  assert (oy' > oy);
  let x =
    float_of_int (x - truncate ox) *. 100. /. (ox' -. ox)
    |> truncate
  and y =
    float_of_int (y - truncate oy) *. 100. /. (oy' -. oy)
    |> truncate
  in
  cartesian_to_polar (x, y) |> f

let wrap_click ev f =
  let f p = f (snd p) in
  wrap_click_aux ev f

let wrap_click_24h ev f_e f_b =
  let f (r, e) = f_b r; f_e e in
  wrap_click_aux ev f

}}

{shared{
   let make_hours_signal h is_am =
     angle_to_hours h |> convert_24h is_am

let make_hm_signal e is_am =
  let h, m = angle_to_hours_minutes e in
  let h = convert_24h is_am h in
  h, m

}} ;;

{shared{

   let make_hours_signal =
     Eliom_lib.create_shared_value
       make_hours_signal {{make_hours_signal}} |>
     Eliom_csreact.SharedReact.S.l2

let make_hm_signal =
  Eliom_lib.create_shared_value
    make_hm_signal {{make_hm_signal}} |>
  Eliom_csreact.SharedReact.S.l2

let make_minutes_signal =
  Eliom_lib.create_shared_value
    angle_to_minutes {{angle_to_minutes}} |>
  Eliom_csreact.SharedReact.S.map

}} ;;

{shared{

   let clock_html_wrap s f =
     let a =
       let open Eliom_content.Svg.F in
       [a_class ["ot-tp-clock"; "ot-tp-click-anywhere"];
        a_viewbox ( 0. , 0. , 100. , 100. );
        a_onclick {{ fun ev -> wrap_click ev %f }}]
     in
     Eliom_content.Html5.D.svg ~a [s]

let clock_html_wrap_24h s f_e f_b =
  let a =
    let open Eliom_content.Svg.F in
    [a_class ["ot-tp-clock";
              "ot-tp-clock-24h";
              "ot-tp-click-anywhere"];
     a_viewbox ( 0. , 0. , 100. , 100. );
     a_onclick {{
       fun ev ->
         let step' = React.Step.create () in
         let step =  Some step' in
         let f_b r = %f_b ?step (r <= 35) in
         wrap_click_24h ev (%f_e ?step) f_b;
         React.Step.execute step' }}]
  in
  Eliom_content.Html5.D.svg ~a [s]

let container l = div ~a:[a_class ["ot-tp-container"]] l

let container_24h l =
  div ~a:[a_class ["ot-tp-container"; "ot-tp-container-24h"]] l

let am_pm_toggle () = Ot_toggle.make ~up_txt:"AM" ~down_txt:"PM" ()

let display_hours h =
  let a = [a_class ["ot-tp-display"]] in
  div ~a [pcdata (Printf.sprintf "%d:00" h)]

let angle_signal_of_hours h =
  let h = if h >= 12 then h - 12 else h in
  h * 30

let display_minutes m =
  let a = [a_class ["ot-tp-display"]] in
  div ~a [pcdata (string_of_int m)]

let angle_signal_of_minutes =
  Eliom_lib.create_shared_value (( * ) 6) {{ ( * ) 6 }} |>
  Eliom_csreact.SharedReact.S.map

let display_hours_minutes (h, m) =
  let a = [a_class ["ot-tp-display"]] in
  div ~a [pcdata (Printf.sprintf "%d:%02d" h m)]

let angle_signal_of_hours_minutes (h, m) =
  let h = if h >= 12 then h - 12 else h in
  h * 30 + m / 2

let combine_inputs_hours_minutes e_h e_m is_am =
  let h =
    (match e_h with Some e_h -> e_h | _ -> 0) |>
    angle_to_hours |>
    convert_24h is_am
  and m = angle_to_minutes e_m in
  h, m

let angle_signal_of_hours' (h, _) =
  (if h >= 12 then h - 12 else h) * 30

let angle_signal_of_minutes' (_, m) = m * 6

}}

{shared{

   let display_hours =
     Eliom_lib.create_shared_value display_hours {{display_hours}} |>
     Eliom_csreact.SharedReact.S.map

let angle_signal_of_hours =
  Eliom_lib.create_shared_value
    angle_signal_of_hours {{angle_signal_of_hours}} |>
  Eliom_csreact.SharedReact.S.map

let make_hours () =
  let e, f_e = Eliom_csreact.SharedReact.S.create 0
  and c, is_am = am_pm_toggle () in
  let h = make_hours_signal e is_am in
  let e = angle_signal_of_hours h in
  let g = clock_html_wrap (clock_svg ~zero_is_12:true e) f_e
  and d = display_hours h |> r_node in
  container [g; c; d], h

let make_hours_24h () =
  let e, f_e = Eliom_csreact.SharedReact.S.create 0
  and b, f_b = Eliom_csreact.SharedReact.S.create true in
  let h = make_hours_signal e b in
  let e = angle_signal_of_hours h in
  let g = clock_html_wrap_24h (clock_svg_24h b e) f_e f_b
  and d = display_hours h |> r_node in
  container_24h [g; d], h

let display_minutes =
  Eliom_lib.create_shared_value display_minutes {{display_minutes}} |>
  Eliom_csreact.SharedReact.S.map

let make_minutes () =
  let e, f_e = Eliom_csreact.SharedReact.S.create 0 in
  let m = make_minutes_signal e in
  let e = angle_signal_of_minutes m in
  let g = clock_html_wrap (clock_svg ~n:12 ~step:5 e) f_e
  and d = display_minutes m |> r_node in
  container [g; d], m

let display_hours_minutes =
  Eliom_lib.create_shared_value
    display_hours_minutes {{display_hours_minutes}} |>
  Eliom_csreact.SharedReact.S.map

let angle_signal_of_hours_minutes =
  Eliom_lib.create_shared_value
    angle_signal_of_hours_minutes {{angle_signal_of_hours_minutes}} |>
  Eliom_csreact.SharedReact.S.map

let make_hours_minutes () =
  let e, f_e = Eliom_csreact.SharedReact.S.create 0
  and c, is_am = am_pm_toggle () in
  let hm = make_hm_signal e is_am in
  let e = angle_signal_of_hours_minutes hm in
  let g =
    let svg = clock_svg e in
    clock_html_wrap svg f_e
  and d = display_hours_minutes hm |> r_node in
  container [g; c; d], hm

let combine_inputs_hours_minutes =
  Eliom_csreact.SharedReact.S.l3
    (Eliom_lib.create_shared_value
       combine_inputs_hours_minutes
       {{combine_inputs_hours_minutes}})

let angle_signal_of_hours' =
  Eliom_lib.create_shared_value
    angle_signal_of_hours' {{angle_signal_of_hours'}} |>
  Eliom_csreact.SharedReact.S.map

let angle_signal_of_minutes' =
  Eliom_lib.create_shared_value
    angle_signal_of_minutes' {{angle_signal_of_minutes'}} |>
  Eliom_csreact.SharedReact.S.map

}}

{shared{

   let make_hours_minutes_seq () =
     let e_h, f_e_h = Eliom_csreact.SharedReact.S.create None
     and e_m, f_e_m = Eliom_csreact.SharedReact.S.create 0 in
     let f_e_h =
       Eliom_lib.create_shared_value
         (fun ?step x ->
            (Eliom_csreact.Shared.local f_e_h) ?step (Some x))
         {{ fun ?step x -> %f_e_h ?step (Some x) }}
     in
     let c, is_am = am_pm_toggle () in
     let hm = combine_inputs_hours_minutes e_h e_m is_am in
     let e_h' = angle_signal_of_hours' hm in
     let g_h =
       clock_html_wrap (clock_svg ~zero_is_12:true e_h') f_e_h
     and d = display_hours_minutes hm |> r_node in
     let r = container [g_h; c; d] in
     {unit{
        let f h =
          let r = Eliom_content.Html5.To_dom.of_div %r in
          r##firstChild >>! fun g_h ->
          let g_m =
            let e_m = angle_signal_of_minutes' %hm in
            clock_html_wrap (clock_svg ~n:12 ~step:5 e_m) %f_e_m |>
            Eliom_content.Html5.To_dom.of_node in
          Lwt.async (fun () ->
            lwt () = Lwt_js.sleep 0.3 in
            Dom.replaceChild r g_m g_h;
            Lwt.return ())
        in
        React.E.map f (React.S.changes %e_h) |> ignore }} |> ignore;
     let go_back = {unit -> unit{
       fun () ->
         let r = Eliom_content.Html5.To_dom.of_div %r
         and g_h = Eliom_content.Html5.To_dom.of_node %g_h in
         r##firstChild >>! Dom.replaceChild r g_h }}
     in
     r, hm, go_back

 }}
