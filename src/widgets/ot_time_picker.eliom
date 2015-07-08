{shared{

open Eliom_content

open Html5.F

type polar = int * int

type cartesian = int * int

type time_receiver = (int -> int -> unit) Eliom_lib.client_value

type 'a rf = ?step:React.step -> int -> unit

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
  and h =
    let h = atan2 y x *. 180. /. pi |> round |> int_of_float in
    (h + 450) mod 360
  in
  r, h

let cartesian_to_angle (x, y) =
  cartesian_to_polar (x, y) |> snd

let angle_to_hours_minutes h =
  let minutes = h * 2 in
  let hours = minutes / 60
  and minutes = minutes mod 60 in
  assert (0 <= hours && hours <= 11);
  assert (0 <= minutes && minutes <= 59);
  hours, minutes

let angle_to_hours h =
  let hours, minutes = angle_to_hours_minutes h in
  let hours =
    if minutes > 30 then
      hours + 1
    else
      hours
  in
  if hours = 12 then 0 else hours

let angle_to_minutes h =
  let minutes = h / 6 in
  assert (0 <= minutes && minutes <= 59);
  if minutes = 60 then 0 else minutes

let cartesian_to_hours_minutes (x, y) =
  cartesian_to_polar (x, y) |> snd |> angle_to_hours_minutes

(* build SVG *)

let cartesian_of_i ?radius:(radius = 40) dh i =
  let h = i * dh in
  polar_to_cartesian (radius, h)

let clock_reactive_hand v_f =
  {[> Svg_types.path ] Eliom_content.Svg.elt{
    let v, f = %v_f in
    let display h =
      let a =
        let s =
          let x, y = polar_to_cartesian (31, h) in
          Printf.sprintf "M %d %d L %d %d"
            center_x center_y
            x y
        in
        [Eliom_content.Svg.F.a_class ["ot-tp-hand"];
         Eliom_content.Svg.F.a_d s]
      in
      Eliom_content.Svg.F.path ~a []
    in
    Eliom_content.Svg.R.node (React.S.map display v)
  }}

let clock_reactive_hand_circle v_f =
  {[> Svg_types.circle ] Eliom_content.Svg.elt{
    let open Eliom_content.Svg.F in
    let v, f = %v_f in
    let display h =
      let a =
        let x, y = polar_to_cartesian (40, h) in
        [a_class ["ot-tp-hand-circle"];
         a_r (float_of_int 9, Some `Px);
         a_cx (float_of_int x, Some `Px);
         a_cy (float_of_int y, Some `Px)]
      in
      circle ~a [title (pcdata "" )]
    in
    Eliom_content.Svg.R.node (React.S.map display v)
  }}

let clock_svg
    ?extra_attributes:(extra_attributes = [])
    ?n:(n = 12)
    ?step:(step = 1)
    ?radius:(radius = 7)
    v_f =
  assert (n >= 0 && 360 mod n = 0);
  let open Eliom_content.Svg.F in
  let dh = 360 / n in
  let l =
    let f i =
      let x, y = cartesian_of_i dh i in
      let a =
        a_class ["ot-tp-text"]
        :: a_dominant_baseline `Central
        :: a_text_anchor `Middle
        :: a_x_list [float_of_int x, Some `Px]
        :: a_y_list [float_of_int y, Some `Px]
        :: extra_attributes
      in
      Eliom_content.Svg.F.text ~a
        [pcdata (string_of_int (step * i))]
    in list_init n f
  in
  let h1 = clock_reactive_hand v_f
  and h2 = clock_reactive_hand_circle v_f in
  let l =
    Eliom_content.Svg.C.node h1
    :: Eliom_content.Svg.C.node h2
    :: l in
  g l

let convert_24h is_am h = if is_am then h else h + 12

}} ;;

{client{

    let (>>!) = Js.Opt.iter

let wrap_click ev f =
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
  cartesian_to_angle (x, y) |> f

let make_hours_signal v_f is_am f =
  let h =
    let f h b = angle_to_hours h |> convert_24h b in
    React.S.l2 f (fst v_f) is_am
  in
  ignore (React.E.map f (React.S.changes h));
  h

let make_minutes_signal v_f f =
  let m = React.S.map angle_to_minutes (fst v_f) in
  ignore (React.E.map f (React.S.changes m));
  m

let make_hm_signal v_f is_am f =
  let (hm : (int * int) React.signal) =
    let f h b =
      let h, m = angle_to_hours_minutes h in
      let h = convert_24h b h in
      h, m
    in
    React.S.l2 f (fst v_f) is_am
  in
  ignore (React.E.map f (React.S.changes hm));
  hm

}} ;;

{shared{

    let clock_html_wrap s v_f =
      let a =
        let open Eliom_content.Svg.F in
        [a_class ["ot-tp-clock"; "ot-tp-click-anywhere"];
         a_viewbox ( 0. , 0. , 100. , 100. );
         a_onclick {{ fun ev -> wrap_click ev (snd %v_f) }}]
      in
      Eliom_content.Html5.F.svg ~a [s]

let container l = div ~a:[a_class ["ot-tp-container"]] l

let am_pm_toggle () = Ot_toggle.make ~up_txt:"AM" ~down_txt:"PM" ()

let display_hours h =
  {{
    (let f h =
       let a = [a_class ["ot-tp-display"]] in
       div ~a [pcdata (Printf.sprintf "%d:00" h)]
     in
     React.S.map f %h) |> Eliom_content.Html5.R.node
  }} |> Eliom_content.Html5.C.node

let make_hours f =
  let v_f = {int rp{ React.S.create 0 }}
  and c, is_am = am_pm_toggle () in
  let g = clock_html_wrap (clock_svg v_f) v_f
  and d =
    {int React.signal{ make_hours_signal %v_f %is_am %f }} |>
    display_hours
  in
  container [g; c; d]

let display_minutes m =
  {{
    (let f m =
       let a = [a_class ["ot-tp-display"]] in
       div ~a [pcdata (string_of_int m)]
     in
     React.S.map f %m) |> Eliom_content.Html5.R.node
  }} |> Eliom_content.Html5.C.node

let make_minutes f =
  let v_f = {int rp{ React.S.create 0 }} in
  let g = clock_html_wrap (clock_svg ~n:12 ~step:5 v_f) v_f
  and d =
    {int React.signal{ make_minutes_signal %v_f %f }} |>
    display_minutes
  in
  container [g; d]

let display_hours_minutes hm =
  {{
    (let f (h, m) =
       let a = [a_class ["ot-tp-display"]] in
       div ~a [pcdata (Printf.sprintf "%d:%02d" h m)]
     in
     React.S.map f %hm) |> Eliom_content.Html5.R.node
  }} |> Eliom_content.Html5.C.node

let display_hours_minutes hm =
  {{
    (let f (h, m) =
       let a = [a_class ["ot-tp-display"]] in
       div ~a [pcdata (Printf.sprintf "%d:%02d" h m)]
     in
     React.S.map f %hm) |> Eliom_content.Html5.R.node
  }} |> Eliom_content.Html5.C.node

let make_hours_minutes f =
  let v_f = {int rp{ React.S.create 0 }}
  and c, is_am = am_pm_toggle () in
  let g = clock_html_wrap (clock_svg v_f) v_f
  and d =
    {(int * int) React.signal{ make_hm_signal %v_f %is_am %f }} |>
    display_hours_minutes
  in
  container [g; c; d]

}}

{client{

    let combine_inputs_hours_minutes =
      (fun h_h h_m is_am ->
         let h = angle_to_hours h_h |> convert_24h is_am
         and m = angle_to_minutes h_m in
         h, m) |> React.S.l3

    let make_hours_minutes_seq () =
      let ((h_h, f_h_h) as v_h_h) = React.S.create 0
      and ((h_m, f_h_m) as v_h_m) = React.S.create 0
      and c, is_am = am_pm_toggle () in
      let g_h = clock_html_wrap (clock_svg v_h_h) v_h_h
      and d =
        combine_inputs_hours_minutes h_h h_m is_am |>
        display_hours_minutes
      in
      let r =
        container [g_h; c; d] |>
        Eliom_content.Html5.To_dom.of_div
      in
      let _ =
        let f h =
          r##firstChild >>! fun g_h ->
          let g_m =
            clock_html_wrap (clock_svg ~n:12 ~step:5 v_h_m) v_h_m |>
            Eliom_content.Html5.To_dom.of_node in
          Dom.replaceChild r g_m g_h
        in
        React.E.map f (React.S.changes h_h)
      in
      Eliom_content.Html5.Of_dom.of_div r

}}

{server{

    let make_hours_minutes_seq () =
      {Html5_types.div Eliom_content.Html5.elt{
          make_hours_minutes_seq ()
        }} |> Eliom_content.Html5.C.node

  }}
