{shared{

open Eliom_content

open Html5.F

type polar = int * int

type cartesian = int * int

type time_receiver = (int -> int -> unit) Eliom_lib.client_value

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

let polar_to_cartesian (cx, cy) (r, h) =
  let h = h *. 3.14159 /. 180. in
  let x = cx + int_of_float (r *. sin h)
  and y = cy - int_of_float (r *. cos h) in
  x, y

let pi = 3.14159

let cartesian_to_polar (cx, cy) (x, y) =
  let x = float_of_int (x - cx)
  and y = float_of_int (y - cy) in
  let r = hypot x y |> round |> int_of_float
  and h =
    let h = atan2 y x *. 180. /. pi |> round |> int_of_float in
    (h + 450) mod 360
  in
  r, h

let polar_to_hours_minutes ((_, h) : polar) =
  let minutes = h * 2 in
  let hours = minutes / 60
  and minutes = minutes mod 60 in
  assert (0 <= hours && hours <= 11);
  assert (0 <= minutes && minutes <= 59);
  hours, minutes

let polar_to_hours p =
  let hours, minutes = polar_to_hours_minutes p in
  let hours =
    if minutes > 30 then
      hours + 1
    else
      hours
  in
  if hours = 12 then 0 else hours

let polar_to_minutes ((_, h) : polar) =
  let minutes = h / 6 in
  if minutes = 60 then 0 else minutes

let cartesian_to_hours_minutes (cx, cy) (x, y) =
  cartesian_to_polar (cx, cy) (x, y) |> polar_to_hours_minutes

(* build SVG *)

let clock_radius = 40.

let cartesian_of_i dh i =
  let h = i * dh |> float_of_int in
  polar_to_cartesian (50, 50) (clock_radius, h)

let clock_svg_circles
    ?extra_attributes:(extra_attributes = [])
    ?n:(n = 12)
    ?step:(step = 1)
    ?radius:(radius = 7)
    () =
  assert ((n >= 0) && (360 mod n) = 0);
  let open Eliom_content.Svg.F in
  let dh = 360 / n in
  let f i =
    let x, y = cartesian_of_i dh i in
    let a =
      a_class ["ot-tp-point"]
      :: a_r (float_of_int radius, Some `Px)
      :: a_cx (float_of_int x, Some `Px)
      :: a_cy (float_of_int y, Some `Px)
      :: extra_attributes
    in
    circle ~a [title (pcdata (string_of_int (step * i)))]
  in
  g (list_init n f)

let clock_svg
    ?extra_attributes:(extra_attributes = [])
    ?n:(n = 12)
    ?step:(step = 1)
    ?radius:(radius = 7)
    () =
  assert ((n >= 0) && (360 mod n) = 0);
  let open Eliom_content.Svg.F in
  let dh = 360 / n in
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
    Eliom_content.Svg.F.text ~a [pcdata (string_of_int (step * i))]
  in
  g (list_init n f)

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
        ?extra_classes:(extra_classes = [])
        ?extra_attributes:(extra_attributes = []) s =
      let open Eliom_content.Svg.F in
      (* let s = svg [s] in *)
      let a =
        a_class ("ot-tp-clock" :: extra_classes)
        :: a_viewbox ( 0. , 0. , 100. , 100. )
        :: extra_attributes
      in
      Eliom_content.Html5.F.svg ~a [s]

  }} ;;

{client{

    let convert_24h is_am h =
      if Eliom_csreact.React.S.value is_am then
        h
      else
        h + 12

}} ;;

{shared{

    let container l =
      div ~a:[a_class ["ot-tp-container"]] l

let make_hours f =
  let c, is_am = Ot_toggle.make ~up_txt:"AM" ~down_txt:"PM" () in
  let extra_attributes =
    let f =
      {int -> int -> unit{
          fun x y ->
            let h =
              cartesian_to_polar (50, 50) (x, y) |>
              polar_to_hours
            in
            assert (h >= 0 && h <= 11);
            let h = convert_24h %is_am h in
            %f h
        }}
    in
    [Eliom_content.Svg.F.a_onclick {{wrap_f_for_onclick %f}}]
  and extra_classes = ["ot-tp-click-anywhere"] in
  container [html_wrap_svg
               ~extra_attributes ~extra_classes
               (clock_svg ());
             c]

let make_minutes f =
  let extra_attributes =
    let f =
      {int -> int -> unit{
          fun x y ->
            let m =
              cartesian_to_polar (50, 50) (x, y) |>
              polar_to_minutes
            in
            assert (m >= 0 && m <= 59);
            %f m
        }}
    in
    [Eliom_content.Svg.F.a_onclick {{wrap_f_for_onclick %f}}]
  and extra_classes = ["ot-tp-click-anywhere"] in
  container
    [html_wrap_svg ~extra_attributes ~extra_classes
       (clock_svg ~n:12 ~step:5 ~radius:2 ())]

let make_continuous_aux f =
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
  and extra_classes = ["ot-tp-click-anywhere"] in
  html_wrap_svg ~extra_attributes ~extra_classes (clock_svg ())

let make_discrete_aux (f : time_receiver) =
  let f = {{
      fun ev ->
        let (>>!) = Js.Opt.iter in
        ev##currentTarget >>! fun x ->
        x##textContent >>! fun x ->
        %f (int_of_string (Js.to_string x)) 0
    }}
  in
  let extra_attributes = [Eliom_content.Svg.F.a_onclick f] in
  html_wrap_svg (clock_svg ~extra_attributes ())

let make_hours_minutes ?discrete:(discrete = true) f =
  let c, is_am = Ot_toggle.make ~up_txt:"AM" ~down_txt:"PM" () in
  let f = {{ fun h m -> let h = convert_24h %is_am h in %f h m }} in
  div ~a:[a_class ["ot-tp-container"]]
    [(if discrete then
        make_discrete_aux f
      else
        make_continuous_aux f);
     c]

}}
