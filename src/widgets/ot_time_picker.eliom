{shared{

open Eliom_content

open Html5.F

type polar = float * float

type cartesian = int * int

type time_receiver = (int -> int -> unit) Eliom_lib.client_value

let polar_to_cartesian (cx, cy) (r, h) =
  let h = h *. 3.14159 /. 180. in
  let x = cx + int_of_float (r *. cos h)
  and y = cy + int_of_float (r *. sin h) in
  x, y

let cartesian_to_polar (cx, cy) (x, y) =
  let x = float_of_int (x - cx)
  and y = float_of_int (y - cy) in
  hypot x y,
  atan2 y x *. 180. /. 3.14159

let polar_to_hours_minutes ((_, h) : polar) =
  let hours =
    if h >= 0. then
      3. +. h /. 30.
    else if h >= -. 90. then
      (90. +. h) /. 30.
    else
      9. +. (180. +. h) /. 30.
  in
  let floor_hours = floor hours in
  let minutes = hours -. floor_hours in
  let hours = int_of_float hours
  and minutes = int_of_float (minutes *. 60.) in
  assert (0 <= hours && hours <= 11);
  assert (0 <= minutes && minutes <= 59);
  hours, minutes

let cartesian_to_hours_minutes (cx, cy) (x, y) =
  cartesian_to_polar (cx, cy) (x, y) |> polar_to_hours_minutes

let clock_svg
    ?extra_attributes:(extra_attributes = [])
    x =
  assert (x > 0);
  assert (x mod 10 = 0);
  let open Eliom_content.Svg.F in
  let c = x / 2
  and r = float_of_int (4 * x / 10) in
  let f i h =
    let h = float_of_int h in
    let x, y = polar_to_cartesian (c, c) (r, h) in
    let a =
      [a_class ["hour-point"];
       a_cx (float_of_int x, Some `Px);
       a_cy (float_of_int y, Some `Px);
       a_r (7., Some `Percent)]
      @ extra_attributes
    in
    circle ~a [title (pcdata (string_of_int i))]
  and l = [270; 300; 330; 0; 30; 60; 90; 120; 150; 180; 210; 240] in
  g (List.mapi f l)

}} ;;

{client{

    let wrap_f_for_onclick f ev =
      let f d =
        let r = d##getBoundingClientRect () in
        let ox = truncate r##left
        and oy = truncate r##top
        and x = ev##clientX
        and y = ev##clientY in
        f (x - ox) (y - oy)
      in
      Js.Opt.iter (ev##currentTarget) f

  }} ;;

{shared{

    let html_wrap_svg
        ?extra_attributes:(extra_attributes = []) s w h =
      let open Eliom_content.Svg.F in
      (* let s = svg [s] in *)
      let a =
        [a_class ["time-picker"];
         a_width (float_of_int w, Some `Px);
         a_height (float_of_int h, Some `Px)]
        @ extra_attributes
      in
      Eliom_content.Html5.F.svg ~a [s]

}} ;;

{shared{

    let time_picker_discrete a (f : time_receiver) =
      let f = {{
          fun ev ->
            let (>>!) = Js.Opt.iter in
            ev##currentTarget >>! fun x ->
            x##firstChild >>! fun x ->
            x##firstChild >>! fun x ->
            x##nodeValue >>! fun x ->
            %f (int_of_string (Js.to_string x)) 0
        }}
      in
      let extra_attributes = [Eliom_content.Svg.F.a_onclick f] in
      html_wrap_svg (clock_svg ~extra_attributes a) a a

let time_picker_continuous a f =
  let extra_attributes =
    let a_over_two = a / 2 in
    let f =
      {int -> int -> unit{
          fun x y ->
            let h, m =
              cartesian_to_hours_minutes
                (%a_over_two, %a_over_two)
                (x, y)
            in
            %f h m }}
    in
    [Eliom_content.Svg.F.a_onclick {{wrap_f_for_onclick %f}}]
  in
  html_wrap_svg ~extra_attributes (clock_svg a) a a

let time_picker ?discrete:(discrete = true) a f =
  assert (a > 0);
  assert (a mod 10 = 0);
  if discrete then
    time_picker_discrete a f
  else
    time_picker_continuous a f

}}
