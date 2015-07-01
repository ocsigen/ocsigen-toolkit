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
    () =
  let open Eliom_content.Svg.F in
  let r = float_of_int 40 in
  let f i h =
    let h = float_of_int h in
    let x, y = polar_to_cartesian (50, 50) (r, h) in
    let a =
      a_class ["ot-tp-point"]
      :: a_cx (float_of_int x, Some `Px)
      :: a_cy (float_of_int y, Some `Px)
      :: a_r (7., Some `Px)
      :: extra_attributes
    in
    circle ~a [title (pcdata (string_of_int i))]
  and l = [270; 300; 330; 0; 30; 60; 90; 120; 150; 180; 210; 240] in
  g (List.mapi f l)

}} ;;

{client{

    let wrap_f_for_onclick f ev =
      let (>>!) = Js.Opt.iter in
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
      f
        (truncate
           (float_of_int (x - truncate ox) *. 100. /. (ox' -. ox)))
        (truncate
           (float_of_int (y - truncate oy) *. 100. /. (oy' -. oy)))


  }} ;;

{shared{

    let html_wrap_svg
        ?extra_attributes:(extra_attributes = []) s =
      let open Eliom_content.Svg.F in
      (* let s = svg [s] in *)
      let a =
        a_class ["ot-tp-clock"]
        :: a_viewbox ( 0. , 0. , 100. , 100. )
        :: extra_attributes
      in
      Eliom_content.Html5.F.svg ~a [s]

}} ;;

{shared{

    let time_picker_discrete (f : time_receiver) =
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
      html_wrap_svg (clock_svg ~extra_attributes ())

let time_picker_continuous f =
  let extra_attributes =
    let f =
      {int -> int -> unit{
          fun x y ->
            let h, m =
              cartesian_to_hours_minutes
                (50, 50)
                (x, y)
            in
            %f h m }}
    in
    [Eliom_content.Svg.F.a_onclick {{wrap_f_for_onclick %f}}]
  in
  html_wrap_svg ~extra_attributes (clock_svg ())


let make ?discrete:(discrete = true) f =
  let c, is_pm = Ot_toggle.make ~up_txt:"PM" ~down_txt:"AM" () in
  let f =
    {{
      fun h m ->
        let h =
          if Eliom_csreact.React.S.value %is_pm then
            h + 12
          else
            h
        in
        %f h m
    }}
  in
  div ~a:[a_class ["ot-tp-container"]]
    [(if discrete then
        time_picker_discrete f
      else
        time_picker_continuous f);
     c]

}}
