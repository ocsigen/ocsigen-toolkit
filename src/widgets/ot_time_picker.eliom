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
  let r = float_of_int r
  and h = float_of_int h in
  let h = h *. 3.14159 /. 180. in
  let x = cx + int_of_float (r *. sin h)
  and y = cy - int_of_float (r *. cos h) in
  x, y

}} ;;

{shared{

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

let cartesian_of_i ?radius:(radius = 40) dh i =
  let h = i * dh in
  polar_to_cartesian (50, 50) (radius, h)

let clock_reactive_hand v_f =
  {[> Svg_types.path ] Eliom_content.Svg.elt{
    let v, f = %v_f in
    let display h =
      let a =
        let s =
          let x, y = polar_to_cartesian (50, 50) (31, h) in
          Printf.sprintf "M 50 50 L %d %d" x y
        in
        [Eliom_content.Svg.F.a_class ["ot-tp-hand"];
         Eliom_content.Svg.F.a_d s]
      in
      Eliom_content.Svg.F.path ~a []
    in
    Eliom_content.Svg.R.node (Eliom_csreact.React.S.map display v)
  }}

let clock_reactive_hand_circle v_f =
  {[> Svg_types.circle ] Eliom_content.Svg.elt{
    let open Eliom_content.Svg.F in
    let v, f = %v_f in
    let display h =
      let a =
        let x, y = polar_to_cartesian (50, 50) (40, h) in
        [a_class ["ot-tp-hand-circle"];
         a_r (float_of_int 9, Some `Px);
         a_cx (float_of_int x, Some `Px);
         a_cy (float_of_int y, Some `Px)]
      in
      circle ~a [title (pcdata "" )]
    in
    Eliom_content.Svg.R.node (Eliom_csreact.React.S.map display v)
  }}

let clock_svg
    ?extra_attributes:(extra_attributes = [])
    ?n:(n = 12)
    ?step:(step = 1)
    ?radius:(radius = 7)
    () =
  assert ((n >= 0) && (360 mod n) = 0);
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
  let v_f =
    {int React.signal * (?step:React.step -> int -> unit){
        Eliom_csreact.React.S.create 0 }}
  in
  let h1 = clock_reactive_hand v_f
  and h2 = clock_reactive_hand_circle v_f
  and f = {?step:React.step -> int -> unit{ snd %v_f }} in
  g (Eliom_content.Svg.C.node h1 ::
     Eliom_content.Svg.C.node h2 ::
     l), f

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
  let g, f' = clock_svg () in
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
            %f' (h * 30);
            let h = convert_24h %is_am h in
            %f h }}
    in
    [Eliom_content.Svg.F.a_onclick {{wrap_f_for_onclick %f}}]
  and extra_classes = ["ot-tp-click-anywhere"] in
  container [html_wrap_svg
               ~extra_attributes ~extra_classes
               g;
             c]

let make_minutes f =
  let g, f' = clock_svg () ~n:12 ~step:5 in
  let extra_attributes =
    let f =
      {int -> int -> unit{
          fun x y ->
            let m =
              cartesian_to_polar (50, 50) (x, y) |>
              polar_to_minutes
            in
            assert (m >= 0 && m <= 59);
            %f' (m * 6);
            %f m
        }}
    in
    [Eliom_content.Svg.F.a_onclick {{wrap_f_for_onclick %f}}]
  and extra_classes = ["ot-tp-click-anywhere"] in
  container [html_wrap_svg ~extra_attributes ~extra_classes g]

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
  html_wrap_svg (fst (clock_svg ~extra_attributes ()))

let make_hours_minutes f =
  let g, f' = clock_svg () in
  let c, is_am = Ot_toggle.make ~up_txt:"AM" ~down_txt:"PM" () in
  let extra_attributes =
    let f =
      {int -> int -> unit{
          fun x y ->
            let h, m =
              cartesian_to_polar (50, 50) (x, y) |>
              polar_to_hours_minutes
            in
            assert (h >= 0 && h <= 11);
            assert (m >= 0 && m <= 59);
            %f' ((h * 60 + m) / 2);
            let h = convert_24h %is_am h in
            %f h m }}
    in
    [Eliom_content.Svg.F.a_onclick {{wrap_f_for_onclick %f}}]
  and extra_classes = ["ot-tp-click-anywhere"] in
  container [html_wrap_svg
               ~extra_attributes ~extra_classes
               g;
             c]

}}
