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

[%%shared.start] (* shared by default, override as necessary *)

open Eliom_shared.React.S.Infix

open Eliom_content.Html

type polar = int * int

type cartesian = int * int

type time_receiver = (int -> int -> unit) Eliom_client_value.t

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

let%client round_lb = -.(2. ** 52.)

let%client round_ub = 2. ** 52.

let%client round x =
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

let%client pi = 3.14159

let%client cartesian_to_polar (x, y) =
  let x = float_of_int (x - center_x)
  and y = float_of_int (y - center_y) in
  let r = hypot x y |> round |> int_of_float
  and e =
    let e = atan2 y x *. 180. /. pi |> round |> int_of_float in
    (e + 450) mod 360
  in
  r, e

let round_5_minutes ?round_5:(round_5 = false) m =
  if round_5 then
    let m = m mod 60 in
    let m =
      if m mod 5 > 2 then
        (m / 5) * 5 + 5
      else
        m / 5 * 5
    in
    if m >= 60 then 55 else m
  else
    m

let round_30_degrees e =
  let e = e mod 360 in
  let e =
    if e mod 30 > 15 then
      (e / 30) * 30 + 30
    else
      e / 30 * 30
  in
  if e >= 360 then 330 else e

let angle_to_hours_minutes ?round_5:(round_5 = false) e =
  let m = e * 2 in
  let h = m / 60
  and m =
    let m = m mod 60 in
    if round_5 then
      if m mod 5 > 2 then
        (m / 5) * 5 + 5
      else
        m / 5 * 5
    else
      m
  in
  let m =
    if m >= 60 then
      if round_5 then 55 else 59
    else
      m
  in
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

let angle_to_minutes_round e =
  let m = e / 30 * 5 in
  assert (0 <= m && m <= 59);
  if e mod 30 >= 15 && m <= 50 then
    m + 5
  else
    m

let angle_to_minutes ?round_5:(round_5 = false) e =
  if round_5 then
    angle_to_minutes_round e
  else
    angle_to_minutes e

let cartesian_of_i ?radius:(radius = 40) de i =
  let e = i * de in
  polar_to_cartesian (radius, e)

let clock_reactive_hand
    ?radius:(radius = Eliom_shared.React.S.const 31)
    e =
  let a =
    let d =
      Eliom_shared.React.S.l2 [%shared
        (fun r e ->
           let x, y = polar_to_cartesian (r, e) in
           Printf.sprintf "M %d %d L %d %d"
             ~%center_x ~%center_y x y
           : int -> int -> string)
      ] radius e
    in
    Eliom_content.Svg.[D.a_class ["ot-tp-hand"]; R.a_d d]
  in
  Eliom_content.Svg.D.path ~a []

let clock_reactive_hand_circle
    ?radius:(radius = 9)
    ?at_radius:(at_radius = Eliom_shared.React.S.const 40)
    e =
  let a =
    let cx =
      Eliom_shared.React.S.l2 [%shared
        ((fun r e ->
           let x, _ = polar_to_cartesian (r, e) in
           float_of_int x, Some `Px)
         : int -> int -> Svg_types.Unit.length)
      ] at_radius e
    and cy =
      Eliom_shared.React.S.l2 [%shared
        ((fun r e ->
           let _, y = polar_to_cartesian (r, e) in
           float_of_int y, Some `Px)
         : int -> int -> Svg_types.Unit.length)
      ] at_radius e
    in
    Eliom_content.Svg.[
      D.a_class ["ot-tp-hand-circle"];
      D.a_r (float_of_int radius, Some `Px);
      R.a_cx cx;
      R.a_cy cy
    ]
  in
  Eliom_content.Svg.(D.circle ~a [D.title (D.pcdata "")])

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
    (e : int Eliom_shared.React.S.t) =
  assert (n >= 0 && 360 mod n = 0);
  let de = 360 / n in
  let l =
    make_clock_point ?zero_is_12 extra_attributes 40 de step |>
    list_init n
  and h1 = clock_reactive_hand e
  and h2 = clock_reactive_hand_circle e in
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
    let radius = b >|= [%shared function true, _ -> 24 | _ -> 36 ] in
    clock_reactive_hand ~radius h
  and h2 =
    let at_radius = b >|= [%shared function true, _ -> 30 | _ -> 42 ] in
    clock_reactive_hand_circle ~radius:6 ~at_radius h
  in
  Eliom_content.Svg.D.g (h1 :: h2 :: l1 @ l2)

let convert_24h is_am h = if is_am then h else h + 12

let%client (>>!) = Js.Opt.iter

let%client wrap_touch_aux ev f =
  ev##.currentTarget >>! fun a ->
  let r = a##getBoundingClientRect in
  let ox = r##.left
  and ox' = r##.right
  and oy = r##.top
  and oy' = r##.bottom in
  assert (ox' >= ox);
  assert (oy' >= oy);
  Js.Optdef.iter (ev##.changedTouches)##(item (0)) @@ fun touch0 ->
  let x =
    float_of_int (touch0##.clientX - truncate ox)
    *. 100.
    /. (ox' -. ox)
    |> truncate
  and y =
    float_of_int (touch0##.clientY - truncate oy)
    *. 100.
    /. (oy' -. oy)
    |> truncate
  in
  f (cartesian_to_polar (x, y))

let%client wrap_touch ~ends ev (f : _ rf) =
  wrap_touch_aux ev (fun (_, e) -> f (e, ends))

let%client wrap_touch_24h ~ends ev f_e f_b =
  wrap_touch_aux ev (fun (r, e) -> f_b (r, ends); f_e (e, ends))

let%client wrap_click_aux ev f =
  ev##.currentTarget >>! fun a ->
  let r = a##getBoundingClientRect in
  let ox = r##.left
  and ox' = r##.right
  and oy = r##.top
  and oy' = r##.bottom
  and x = ev##.clientX
  and y = ev##.clientY in
  assert (ox' > ox);
  assert (oy' > oy);
  let x =
    float_of_int (x - truncate ox) *. 100. /. (ox' -. ox)
    |> truncate
  and y =
    float_of_int (y - truncate oy) *. 100. /. (oy' -. oy)
    |> truncate
  in
  f (cartesian_to_polar (x, y))

let%client wrap_click ev (f : _ rf) =
  wrap_click_aux ev (fun (_, e) -> f (e, true))

let%client wrap_click_24h ev f_e f_b =
  wrap_click_aux ev (fun (r, e) -> f_b r; f_e (e, true))

let clock_html_wrap ?(classes = [])
    s (f : (int * bool) rf Eliom_client_value.t) =
  let e =
    let a =
      let open Eliom_content.Svg.F in [
        a_class
          ("ot-tp-clock"
           :: "ot-tp-click-anywhere"
           :: classes);
        a_viewBox ( 0. , 0. , 100. , 100. );
        a_onclick [%client fun ev -> wrap_click ev ~%f ]
      ]
    in
    Eliom_content.Html.D.svg ~a [s]
  in
  let _ = [%client
    (let e = Eliom_content.Html.To_dom.of_element ~%e in
     (Lwt.async @@ fun () ->
      Lwt_js_events.touchends e @@ fun ev _ ->
      Lwt.return (wrap_touch true ev ~%f));
     (Lwt.async @@ fun () ->
      Lwt_js_events.touchcancels e @@ fun ev _ ->
      Lwt.return (wrap_touch true ev ~%f));
     (Lwt.async @@ fun () ->
      Lwt_js_events.touchmoves e @@ fun ev _ ->
      Lwt.return (wrap_touch false ev ~%f))
     : unit)
  ] in
  e

let clock_html_wrap_24h ?(classes = []) s f_e f_b =
  let e =
    let a =
      let f = [%client fun ev ->
        let step' = React.Step.create () in
        let step =  Some step' in
        let f_b r = ~%f_b ?step (r <= 35, true) in
        wrap_click_24h ev (~%f_e ?step) f_b;
        React.Step.execute step'
      ] in
      let open Eliom_content.Svg.F in [
        a_class
          ("ot-tp-clock"
           :: "ot-tp-clock-24h"
           :: "ot-tp-click-anywhere"
           :: classes);
        a_viewBox ( 0. , 0. , 100. , 100. );
        a_onclick f
      ]
    in
    Eliom_content.Html.D.svg ~a [s]
  in
  ignore @@ [%client
    ((let f ~ends ev =
        let step = React.Step.create () in
        let f_b (r, b) = ~%f_b ~step (r <= 35, b) in
        wrap_touch_24h ~ends ev (~%f_e ~step) f_b;
        React.Step.execute step
      and e = Eliom_content.Html.To_dom.of_element ~%e in
      (Lwt.async @@ fun () ->
       Lwt_js_events.touchends e @@ fun ev _ ->
       Lwt.return (f true ev));
      (Lwt.async @@ fun () ->
       Lwt_js_events.touchcancels e @@ fun ev _ ->
       Lwt.return (f true ev));
      (Lwt.async @@ fun () ->
       Lwt_js_events.touchmoves e @@ fun ev _ ->
       Lwt.return (f false ev)))
     : unit)
  ];
  e

let container = D.(div ~a:[a_class ["ot-tp-container"]])

let am_pm_toggle ?init_am:(init_am = true) ?update () =
  let init_up = init_am in
  Ot_toggle.make ~init_up ~up_txt:"AM" ~down_txt:"PM" ?update ()

let string_of_hours ?h24:(h24 = false) h =
  if h24 then
    string_of_int h
  else if h = 0 || h = 12 then
    "12"
  else if h < 12 then
    string_of_int h
  else
    string_of_int (h - 12)

let display_hours_minutes_seq ?h24:(h24 = false) f (h, m) b =
  let h' =
    let a =
      if b then
        [D.a_class ["ot-tp-hours"; "ot-tp-active"]]
      else
        [D.a_class ["ot-tp-hours"; "ot-tp-inactive"];
         D.a_onclick [%client  fun _ -> ~%f true ]]
    and c = [string_of_hours ~h24 h |> D.pcdata] in
    D.span ~a c
  and m =
    let a =
      if b then
        D.[a_class ["ot-tp-minutes"; "ot-tp-inactive"];
           a_onclick [%client  fun _ -> ~%f false ]]
      else
        [D.a_class ["ot-tp-minutes"; "ot-tp-active"]]
    and c = [Printf.sprintf "%02d" m |> D.pcdata] in
    D.span ~a c
  and a = [D.a_class ["ot-tp-display"]] in
  if h24 then
    D.div ~a [h'; D.pcdata ":"; m]
  else
    D.div ~a [h'; D.pcdata ":"; m;
              D.pcdata (if h < 12 then " AM" else " PM")]

let combine_inputs_hours_minutes ?round_5 e_h e_m z_e_h z_e_m is_am =
  let f = [%shared fun (x, b) -> if b then Some x else None ] in
  let e_h = Eliom_shared.React.S.fmap f [%shared ~%z_e_h] e_h
  and e_m = Eliom_shared.React.S.fmap f [%shared ~%z_e_m] e_m in
  Eliom_shared.React.S.l3 [%shared fun e_h e_m is_am ->
    let h = angle_to_hours e_h |> convert_24h is_am
    and m = angle_to_minutes ?round_5:~%round_5 e_m in
    h, m
  ] e_h e_m is_am

let angle_signal_of_hours' =
  Eliom_shared.React.S.map [%shared fun (h, _) ->
    (if h >= 12 then h - 12 else h) * 30
  ]

let angle_signal_of_minutes' =
  Eliom_shared.React.S.map [%shared fun (_, m) -> m * 6 ]

let display_hours_minutes_seq ?h24 f =
  Eliom_shared.React.S.l2 [%shared
    display_hours_minutes_seq ?h24:~%h24 ~%f
  ]

let show_minutes_aux
      ?(action : (int * int -> unit Lwt.t) Eliom_client_value.t option)
      e_m hm f_e_m =
  clock_html_wrap ~classes:["ot-tp-clock-min"]
    (clock_svg ~n:12 ~step:5 e_m)
    [%client
      ((fun ?step m ->
         ~%f_e_m ?step m;
         match ~%action with
         | Some action ->
           let v = React.S.value ~%hm in
           Lwt.async (fun () -> action v)
         | None ->
           ())
       : (int * bool) rf)
    ]

let get_angle_signal ?round =
  Eliom_shared.React.S.map
    [%shared
      fun (e, b) ->
        match b, ~%round with
        | true, Some true ->
          round_30_degrees e
        | _, _ ->
          e
    ]

let make_hours_minutes_seq_24h
    ?(action : (int * int -> unit Lwt.t) Eliom_client_value.t option)
    ?init:(init = (0, 0))
    ?(update : (int * int) React.E.t Eliom_client_value.t option) ?round_5 () =
  let i_h, i_m = init in
  let i_m = round_5_minutes ?round_5 i_m in
  let z_e_h = (i_h mod 12) * 30
  and z_e_m = i_m * 6
  and z_am = i_h < 12 in
  let e_h, f_e_h = Eliom_shared.React.S.create (z_e_h, true)
  and e_m, f_e_m = Eliom_shared.React.S.create (z_e_m, true)
  and is_am, f_is_am = Eliom_shared.React.S.create (z_am, true) in
  (match update with
   | Some update ->
     ignore [%client
       (ignore @@
        let f (h, m) =
          let h, b = if h >= 12 then h - 12, false else h, true in
          ~%f_e_h (h * 30, true);
          ~%f_e_m (m * 6, true);
          ~%f_is_am (b, true)
        in
        React.E.map f ~%update
        : unit)]
   | None ->
     ());
  let b, f_b = Eliom_shared.React.S.create true in
  let hm =
    let is_am =
      Eliom_shared.React.S.fmap
        [%shared fun (x, b) -> if b then Some x else None ]
        [%shared ~%z_am]
        is_am
    in
    combine_inputs_hours_minutes ?round_5 e_h e_m z_e_h z_e_m is_am
  in
  let g_h =
    let e_h' = get_angle_signal ~round:true e_h
    and f_e_h =
      Eliom_shared.Value.create
        (Eliom_shared.Value.local f_e_h)
        [%client fun ?step ((x, b) as p) ->
           ~%f_e_h ?step p;
           if b then ~%f_b false
        ]
    in
    clock_html_wrap_24h
      ~classes:["ot-tp-clock-hr"]
      (clock_svg_24h is_am e_h') f_e_h f_is_am
  in
  let g =
    let e_m = get_angle_signal ?round:round_5 e_m in
    b >|= [%shared fun b ->
      if b then
        ~%g_h
      else
        show_minutes_aux ?action:~%action ~%e_m ~%hm ~%f_e_m
    ] |> R.node
  and d = display_hours_minutes_seq ~h24:true f_b hm b |> R.node in
  container [d; g], hm, [%client ( fun () -> ~%f_b true : unit -> unit)]

let make_hours_minutes_seq
    ?action ?init:(init = (0, 0)) ?update ?round_5 () =
  let i_h, i_m = init in
  let i_m = round_5_minutes ?round_5 i_m in
  let z_e_h = (i_h mod 12) * 30 and z_e_m = i_m * 6 in
  let e_h, f_e_h = Eliom_shared.React.S.create (z_e_h, true)
  and e_m, f_e_m = Eliom_shared.React.S.create (z_e_m, true)
  and b, f_b = Eliom_shared.React.S.create true in
  let c, is_am =
    let update =
      match update with
      | Some update ->
        [%client (
           let f (h, m) =
             (let h = if h >= 12 then h - 12 else h in
              ~%f_e_h (h * 30, true));
             ~%f_e_m (m * 6, true)
           in
           React.E.map f ~%update |> ignore : unit)] |> ignore;
        Some [%client (
          React.E.map (fun (h, _) -> h < 12) ~%update
        : bool React.E.t)]
      | None ->
        None
    in
    am_pm_toggle ?update ~init_am:(i_h < 12) ()
  in
  let hm =
    combine_inputs_hours_minutes ?round_5 e_h e_m z_e_h z_e_m is_am
  in
  let g_h =
    let e_h' = Eliom_shared.React.S.map [%shared fst ] e_h
    and f_e_h = [%client fun ?step ((x, b) as p) ->
      ~%f_e_h ?step p;
      if b then ~%f_b false ]
    in
    clock_html_wrap
      ~classes:["ot-tp-clock-hr"]
      (clock_svg ~zero_is_12:true e_h') f_e_h
  in
  let g =
    let e_m = get_angle_signal ?round:round_5 e_m in
    b >|= [%shared fun b ->
      if b then
        ~%g_h
      else
        show_minutes_aux ?action:~%action ~%e_m ~%hm ~%f_e_m
    ] |> R.node
  and d = display_hours_minutes_seq ~h24:false f_b hm b |> R.node in
  container [d; g; c], hm, [%client ( fun () -> ~%f_b true : unit -> unit)]

let make_hours_minutes_seq
    ?action ?init ?update ?round_5 ?(h24 = true) () =
  (if h24 then
     make_hours_minutes_seq_24h
   else
     make_hours_minutes_seq) ?action ?init ?update ?round_5 ()

let make = make_hours_minutes_seq
