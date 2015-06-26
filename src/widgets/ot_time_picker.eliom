{shared{

open Eliom_content

open Html5.F

type polar = float * float

type cartesian = int * int

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

let clock_svg x =
  assert (x > 0);
  assert (x mod 10 = 0);
  let open Eliom_content.Svg.F in
  let c = x / 2
  and r = float_of_int (4 * x / 10) in
  let f i h =
    let h = float_of_int h in
    let x, y = polar_to_cartesian (c, c) (r, h) in
    circle
      ~a:[a_cx (float_of_int x, Some `Px);
          a_cy (float_of_int y, Some `Px);
          a_r (7., Some `Percent)]
      [title (pcdata (string_of_int i))]
  and l = [270; 300; 330;
           0; 30; 60;
           90; 120; 150;
           180; 210; 240] in
  g (List.mapi f l)

let svg_with_onclick s w h f =
  let open Eliom_content.Svg.F in
  let s = svg [s] in
  Eliom_content.Html5.F.svg
    ~a:[a_width (float_of_int w, Some `Px);
        a_height (float_of_int h, Some `Px);
        a_onclick
          {{
            fun ev ->
              Js.Opt.iter
                (ev##currentTarget)
                (fun d ->
                   let r = d##getBoundingClientRect () in
                   let ox = truncate r##left
                   and oy = truncate r##top
                   and x = ev##clientX
                   and y = ev##clientY in
                   %f (x - ox) (y - oy))
          }}]
    [s]

}} ;;

{client{

let time_picker a f =
  assert (a > 0);
  assert (a mod 10 = 0);
  let a_over_two = a / 2 in
  svg_with_onclick (clock_svg a) a a
    (fun x y ->
       f (cartesian_to_hours_minutes
            (a_over_two, a_over_two)
            (x, y)))

}}
