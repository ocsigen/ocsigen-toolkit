{shared{

open Eliom_content

open Html5.F

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

let clock_reactive_hand
    ?radius:(radius = {int React.signal{ React.S.const 31 }})
    e =
  {[> Svg_types.path ] Eliom_content.Svg.elt{
    let display r e =
      let a =
        let s =
          let x, y = polar_to_cartesian (r, e) in
          Printf.sprintf "M %d %d L %d %d"
            center_x center_y
            x y
        in
        [Eliom_content.Svg.F.a_class ["ot-tp-hand"];
         Eliom_content.Svg.F.a_d s]
      in
      Eliom_content.Svg.F.path ~a []
    in
    Eliom_content.Svg.R.node (React.S.l2 display %radius %e)
  }}

let clock_reactive_hand_circle
    ?radius:(radius = 9)
    ?at_radius:(at_radius = {int React.signal{ React.S.const 40 }})
    e =
  {[> Svg_types.circle ] Eliom_content.Svg.elt{
    let open Eliom_content.Svg.F in
    let display r e =
      let a =
        let x, y = polar_to_cartesian (r, e) in
        [a_class ["ot-tp-hand-circle"];
         a_r (float_of_int %radius, Some `Px);
         a_cx (float_of_int x, Some `Px);
         a_cy (float_of_int y, Some `Px)]
      in
      circle ~a [title (pcdata "" )]
    in
    Eliom_content.Svg.R.node (React.S.l2 display %at_radius %e)
  }}

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
    e =
  assert (n >= 0 && 360 mod n = 0);
  let de = 360 / n in
  let l =
    make_clock_point ?zero_is_12 extra_attributes 40 de step |>
    list_init n
  and h1 = Eliom_content.Svg.C.node (clock_reactive_hand e)
  and h2 = Eliom_content.Svg.C.node (clock_reactive_hand_circle e) in
  Eliom_content.Svg.F.g (h1 :: h2 :: l)

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
    clock_reactive_hand ~radius h |> Eliom_content.Svg.C.node
  and h2 =
    let at_radius =
      {int React.signal{
          let f b = if b then 30 else 42 in
          React.S.map f %b }}
    in
    clock_reactive_hand_circle ~radius:6 ~at_radius h |>
    Eliom_content.Svg.C.node
  in
  Eliom_content.Svg.F.g (h1 :: h2 :: l1 @ l2)

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

let make_hours_signal h is_am =
  let f h b = angle_to_hours h |> convert_24h b in
  React.S.l2 f h is_am

let make_minutes_signal = React.S.map angle_to_minutes

let make_hm_signal e is_am =
  let f e b =
    let h, m = angle_to_hours_minutes e in
    let h = convert_24h b h in
    h, m
  in
  React.S.l2 f e is_am

}} ;;

{shared{

    let react_create_int v =
      let r = {int rp{ React.S.create %v }} in
      {int React.signal{ fst %r }}, {int rf{ snd %r }}

    let react_create_bool v =
      let r = {bool rp{ React.S.create %v }} in
      {bool React.signal{ fst %r }}, {bool rf{ snd %r }}

    let clock_html_wrap s f =
      let a =
        let open Eliom_content.Svg.F in
        [a_class ["ot-tp-clock"; "ot-tp-click-anywhere"];
         a_viewbox ( 0. , 0. , 100. , 100. );
         a_onclick {{ fun ev -> wrap_click ev %f }}]
      in
      Eliom_content.Html5.F.svg ~a [s]

let clock_html_wrap_24h s f_e (f_b : bool rf Eliom_lib.client_value) =
  let a =
    let open Eliom_content.Svg.F in
    [a_class ["ot-tp-clock";
              "ot-tp-clock-24h";
              "ot-tp-click-anywhere"];
     a_viewbox ( 0. , 0. , 100. , 100. );
     a_onclick {{
         fun ev ->
           let step = React.Step.create () in
           let f_b r = %f_b ~step (r <= 35) in
           wrap_click_24h ev (%f_e ~step) f_b;
           React.Step.execute step }}]
  in
  Eliom_content.Html5.F.svg ~a [s]

let container l = div ~a:[a_class ["ot-tp-container"]] l

let container_24h l =
  div ~a:[a_class ["ot-tp-container"; "ot-tp-container-24h"]] l

let am_pm_toggle () = Ot_toggle.make ~up_txt:"AM" ~down_txt:"PM" ()

let display_hours h =
  {{
    (let f h =
       let a = [a_class ["ot-tp-display"]] in
       div ~a [pcdata (Printf.sprintf "%d:00" h)]
     in
     React.S.map f %h) |> Eliom_content.Html5.R.node
  }} |> Eliom_content.Html5.C.node

let angle_signal_of_hours h =
  {int React.signal{
      let f h =
        let h = if h >= 12 then h - 12 else h in
        h * 30
      in
      React.S.map f %h }}

let make_hours () =
  let e, f_e = react_create_int 0
  and c, is_am = am_pm_toggle () in
  let h = {int React.signal{ make_hours_signal %e %is_am }} in
  let e = angle_signal_of_hours h in
  let g = clock_html_wrap (clock_svg ~zero_is_12:true e) f_e
  and d = display_hours h in
  container [g; c; d], h

let make_hours_24h () =
  let e, f_e = react_create_int 0
  and b, f_b = react_create_bool true in
  let h = {int React.signal{ make_hours_signal %e %b }} in
  let e = angle_signal_of_hours h in
  let g = clock_html_wrap_24h (clock_svg_24h b e) f_e f_b
  and d = display_hours h in
  container_24h [g; d], h

let display_minutes m =
  {{
    (let f m =
       let a = [a_class ["ot-tp-display"]] in
       div ~a [pcdata (string_of_int m)]
     in
     React.S.map f %m) |> Eliom_content.Html5.R.node
  }} |> Eliom_content.Html5.C.node

let angle_signal_of_minutes m =
  {int React.signal{ React.S.map (( * ) 6) %m }}

let make_minutes () =
  let e, f_e = react_create_int 0 in
  let m = {int React.signal{ make_minutes_signal %e }} in
  let e = angle_signal_of_minutes m in
  let g = clock_html_wrap (clock_svg ~n:12 ~step:5 e) f_e
  and d = display_minutes m in
  container [g; d], m

let display_hours_minutes hm =
  {{
    (let f (h, m) =
       let a = [a_class ["ot-tp-display"]] in
       div ~a [pcdata (Printf.sprintf "%d:%02d" h m)]
     in
     React.S.map f %hm) |> Eliom_content.Html5.R.node
  }} |> Eliom_content.Html5.C.node

let angle_signal_of_hours_minutes hm =
  {int React.signal{
      let f (h, m) =
        let h = if h >= 12 then h - 12 else h in
        h * 30 + m / 2
      in
      React.S.map f %hm }}

let make_hours_minutes () =
  let e, f_e = react_create_int 0
  and c, is_am = am_pm_toggle () in
  let hm = {(int * int) React.signal{ make_hm_signal %e %is_am }} in
  let e = angle_signal_of_hours_minutes hm in
  let g = clock_html_wrap (clock_svg e) f_e
  and d = display_hours_minutes hm in
  container [g; c; d], hm

}}

{client{

    let combine_inputs_hours_minutes =
      (fun e_h e_m is_am ->
         let h =
           (match e_h with Some e_h -> e_h | _ -> 0) |>
           angle_to_hours |>
           convert_24h is_am
         and m = angle_to_minutes e_m in
         h, m) |> React.S.l3

let angle_signal_of_hours' (hm : (int * int) React.signal) =
  let f (h, _) =
    let h = if h >= 12 then h - 12 else h in
    h * 30
  in
  React.S.map f hm

let angle_signal_of_minutes' (hm : (int * int) React.signal) =
  let f (_, m) = m * 6 in
  React.S.map f hm

let make_hours_minutes_seq () =
  let e_h, f_e_h = React.S.create None
  and e_m, f_e_m = React.S.create 0 in
  let f_e_h ?step x = f_e_h ?step (Some x)
  and c, is_am = am_pm_toggle () in
  let hm = combine_inputs_hours_minutes e_h e_m is_am in
  let e_h' = angle_signal_of_hours' hm in
  let g_h = clock_html_wrap (clock_svg ~zero_is_12:true e_h') f_e_h
  and d = display_hours_minutes hm in
  let r =
    container [g_h; c; d] |>
    Eliom_content.Html5.To_dom.of_div
  in
  let _ =
    let f h =
      r##firstChild >>! fun g_h ->
      let g_m =
        let e_m = angle_signal_of_minutes' hm in
        clock_html_wrap (clock_svg ~n:12 ~step:5 e_m) f_e_m |>
        Eliom_content.Html5.To_dom.of_node in
      Dom.replaceChild r g_m g_h
    in
    React.E.map f (React.S.changes e_h)
  in
  Eliom_content.Html5.Of_dom.of_div r, hm

}}

{server{

    let make_hours_minutes_seq () =
      let p =
        {Html5_types.div Eliom_content.Html5.elt *
         (int * int) React.signal{
           make_hours_minutes_seq () }} in
      Eliom_content.Html5.C.node {{ fst %p }},
      {(int * int) React.signal{ snd %p }}

  }}
