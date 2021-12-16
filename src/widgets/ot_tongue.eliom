[%%shared
open Eliom_content.Html
open Eliom_content.Html.F]

[%%client
open Lwt.Infix
open Js_of_ocaml
open Js_of_ocaml_lwt]

let%client inertia_parameter1 = 0.1
(* WARNING: inertia_parameter1 must be adapted to cubic-bezier in CSS!
   If small (0.1), the elt will slow down very progressively and the movement will
   last for a long time (for a given distance).
   If large (1), the elt will keep its speed during the whole movement
   and stop abruptly.
   If 0.5, the movement will last twice as long
   than if it was keeping its initial speed all along (for the same distance).
   Corresponds to the first argument of cubic-bezier(,,,) if the second one is 1.
   It must be adjusted so that the initial speed of the transition
   matches the current speed when the finger is released.
   dx = average_speed * dt
   dx = initial_speed * dt / inertia_parameter1
*)
let%client inertia_parameter2 = 0.0001
(* Controls the movement duration, which depends on initial speed.
   Higher value, longer movement.
*)
let%client inertia_parameter3 = 0.5
(* Controls the movement duration.
   If large (1.0) high duration for high initial speed.
   dt = (inertia_parameter2 * speed)^inertia_parameter3
*)
type%shared simple_stop = [`Percent of int | `Px of int | `Full_content]

type%shared stop =
  [ `Percent of int
  | `Px of int
  | `Full_content
  | `Interval of simple_stop * simple_stop ]

type%shared tongue =
  { elt : Html_types.div Eliom_content.Html.D.elt
  ; stop_signal_before : simple_stop React.S.t Eliom_client_value.t
  ; stop_signal_after : simple_stop React.S.t Eliom_client_value.t
  ; px_signal_before : int React.S.t Eliom_client_value.t
  ; px_signal_after : int React.S.t Eliom_client_value.t }

let%client now () = (new%js Js.date_now)##getTime /. 1000.

let%client clX ev =
  Js.Optdef.case
    ev ##. changedTouches ## (item 0)
    (fun () -> 0)
    (fun a -> a##.clientX)

let%client clY ev =
  Js.Optdef.case
    ev ##. changedTouches ## (item 0)
    (fun () -> 0)
    (fun a -> a##.clientY)

let%client documentsize vert =
  (if vert then snd else fst) (Ot_size.get_document_size ())

let%client pxb v = Printf.sprintf "translateY(%dpx)" (-v)
let%client pxr v = Printf.sprintf "translateX(%dpx)" (-v)
let%client pxt v = Printf.sprintf "translateY(%dpx)" v
let%client pxl v = Printf.sprintf "translateX(%dpx)" v

let%client perb v =
  Printf.sprintf "translateY(%dpx)" (documentsize true * -v / 100)

let%client perr v =
  Printf.sprintf "translateX(%dpx)" (documentsize false * -v / 100)

let%client pert v =
  Printf.sprintf "translateY(%dpx)" (documentsize true * v / 100)

let%client perl v =
  Printf.sprintf "translateX(%dpx)" (documentsize false * v / 100)

let%client full_size tongue_elt vert =
  if vert
  then (To_dom.of_element tongue_elt)##.scrollHeight
  else (To_dom.of_element tongue_elt)##.scrollWidth

let%client make_stop tongue_elt side stop =
  let vert = side = `Top || side = `Bottom in
  Js.string
  @@
  match side, stop with
  | `Bottom, `Px v -> pxb v
  | `Left, `Px v -> pxl v
  | `Top, `Px v -> pxt v
  | `Right, `Px v -> pxr v
  | `Bottom, `Full_content -> pxb @@ full_size tongue_elt vert
  | `Left, `Full_content -> pxl @@ full_size tongue_elt vert
  | `Top, `Full_content -> pxt @@ full_size tongue_elt vert
  | `Right, `Full_content -> pxr @@ full_size tongue_elt vert
  | `Bottom, `Percent v -> perb v
  | `Left, `Percent v -> perl v
  | `Top, `Percent v -> pert v
  | `Right, `Percent v -> perr v

let%shared class_of_side = function
  | `Top -> "ot-tongue-top"
  | `Bottom -> "ot-tongue-bottom"
  | `Left -> "ot-tongue-left"
  | `Right -> "ot-tongue-right"

let%client get_size ~side elt =
  let w, h = Ot_size.get_document_size () in
  match side with
  | `Top -> int_of_float (Ot_size.client_bottom elt)
  | `Bottom -> h - int_of_float (Ot_size.client_top elt)
  | `Left -> int_of_float (Ot_size.client_right elt)
  | `Right -> w - int_of_float (Ot_size.client_left elt)

let%client px_of_simple_stop vert tongue_elt stop =
  let docsize = documentsize vert in
  match stop with
  | `Px v -> v
  | `Percent v -> v * docsize / 100
  | `Full_content -> full_size tongue_elt vert

(** Convert a list of stops in pixels, for current screen size *)
let%client px_of_stops tongue_elt vert stops =
  let docsize = documentsize vert in
  let px_of_simple_stop interval_info stop acc =
    match stop with
    | `Px v as a -> (v, a, interval_info) :: acc
    | `Percent v as a -> (v * docsize / 100, a, interval_info) :: acc
    | `Full_content ->
        (full_size tongue_elt vert, `Full_content, interval_info) :: acc
  in
  let px_of_stop interval_info (stop : stop) acc =
    match stop with
    | #simple_stop as stop -> px_of_simple_stop interval_info stop acc
    | `Interval (start, stop) ->
        px_of_simple_stop `Start start (px_of_simple_stop `End stop acc)
  in
  let fst3 (x, _, _) = x in
  List.sort (fun a b -> compare (fst3 a) (fst3 b))
  @@ List.fold_right (px_of_stop `Point) stops []

let%client closest_stop ~speed ~maxsize size stops =
  let size =
    let sign = if speed < 0.0 then -1. else 1. in
    min maxsize
      (size
      + int_of_float
          (sign
          *. Float.(
               pow (inertia_parameter2 *. abs speed) (1. +. inertia_parameter3))
          *. inertia_parameter1 /. inertia_parameter2))
  in
  match
    (* Computes the stop with the minimum distance *)
    List.fold_left
      (fun ((closest_d, _, _, _) as closest) (px, stop, interval_info) ->
        let d = abs (size - px) in
        if closest_d < d
        then closest
        else if px <= maxsize
        then d, px, (stop, true), interval_info
        else abs (size - maxsize), maxsize, (`Px maxsize, true), `Point)
      (max_int, 0, (`Px 0, true), `Point)
      stops
  with
  (* If we are in an interval we will return the number of pixels *)
  | _, px, _, `Start when size > px -> `Px size, false
  | _, px, _, `End when size < px -> `Px size, false
  (* Otherwise we will return the stop *)
  | _, _, s, _ -> s

let%client rec stop_after ~maxsize ~speed size stops =
  match stops with
  | [] -> `Px 0, true
  | (_, _, `End) :: _ -> closest_stop ~speed ~maxsize size stops
  | (px, _, _) :: _ when px >= maxsize -> `Px maxsize, false
  | [(_, stop, _)] -> stop, true
  | (px, stop, _) :: _ when px >= size -> stop, true
  | _ :: l -> stop_after ~maxsize ~speed size l

let%client stop_before ~speed ~maxsize size stops =
  let stops = List.rev stops in
  let rec aux = function
    | [] -> `Px 0, true
    | (px, _, `Start) :: _ when px <= size ->
        closest_stop ~speed ~maxsize size stops
    | [(_, stop, _)] -> stop, true
    | (px, stop, _) :: _ when px <= size -> stop, true
    | _ :: l -> aux l
  in
  aux stops

let%client disable_transition elt =
  Manip.Class.add elt "notransition";
  Lwt_js_events.request_animation_frame ()

let%client enable_transition ?duration elt =
  (match duration with
  | None -> ()
  | Some duration ->
      let elt' = To_dom.of_element elt in
      (Js.Unsafe.coerce elt'##.style)##.transitionDuration
      := Js.string (Printf.sprintf "%.2fs" duration));
  Manip.Class.remove elt "notransition";
  Lwt_js_events.request_animation_frame ()

let%client bind side stops init handle update set_before_signal set_after_signal
    elt
  =
  let open Lwt_js_events in
  let elt' = To_dom.of_element elt in
  let defaultduration = (Js.Unsafe.coerce elt'##.style)##.transitionDuration in
  let handle' = To_dom.of_element handle in
  let vert = side = `Top || side = `Bottom in
  let sign = match side with `Top | `Left -> -1 | _ -> 1 in
  let cl = if vert then clY else clX in
  let currentstop = ref init in
  let startpos = ref 0 in
  let currentpos = ref 0 in
  let previouspos = ref 0 in
  let previoustimestamp = ref 0. in
  let startsize = ref 0 (* height or width of visible part in pixel *) in
  let animation_frame_requested = ref false in
  let set speed (stop, is_attractor) =
    let previousstop = !currentstop in
    currentstop := stop;
    let duration =
      if is_attractor
      then None
      else Some Float.(pow (inertia_parameter2 *. abs speed) inertia_parameter3)
    in
    let%lwt () = enable_transition ?duration elt in
    elt'##.style##.transform := make_stop elt side stop;
    set_before_signal stop;
    Lwt.async (fun () ->
        let%lwt () =
          if stop <> previousstop
          then Lwt_js_events.transitionend elt'
          else Lwt.return_unit
        in
        set_after_signal stop; Lwt.return_unit);
    Lwt.return_unit
  in
  Lwt.async (fun () ->
      (* Initialize size *)
      let maxsize = full_size elt vert in
      let px = px_of_simple_stop vert elt init in
      let stop = if px > maxsize then `Px maxsize else init in
      set 0. (stop, true));
  let pos ev =
    try cl ev with _ -> !currentpos
    (* Firefox (at least on Linux) fails to get touchend touchlist ...
       I catch the exception and use !currentpos in that case ... *)
  in
  let speed pos =
    -.float (sign * (pos - !previouspos)) /. (now () -. !previoustimestamp)
  in
  let next_stop speed pos =
    let dpos = sign * (pos - !previouspos) in
    let stops = px_of_stops elt vert stops in
    let newsize = !startsize + (sign * (!startpos - !currentpos)) in
    let maxsize = full_size elt vert in
    if dpos < 0
    then stop_after ~speed ~maxsize newsize stops
    else stop_before ~speed ~maxsize newsize stops
  in
  let ontouchmove ev _ =
    let pos = cl ev in
    if pos <> !currentpos
    then (
      previouspos := !currentpos;
      previoustimestamp := now ();
      currentpos := pos);
    if not !animation_frame_requested
    then (
      animation_frame_requested := true;
      let%lwt () = Lwt_js_events.request_animation_frame () in
      animation_frame_requested := false;
      let d = sign * (!startpos - !currentpos) in
      let maxsize = full_size elt vert in
      let size = min (!startsize + d) maxsize in
      (elt'##.style##.transform
      := Js.string
         @@
         match side with
         | `Bottom -> pxb size
         | `Top -> pxt size
         | `Left -> pxl size
         | `Right -> pxr size);
      Lwt.return_unit)
    else Lwt.return_unit
  in
  let ontouchend ev =
    let pos = pos ev in
    let speed = speed pos in
    set speed (next_stop speed pos)
  in
  let ontouchcancel ev = set (speed (pos ev)) (!currentstop, true) in
  let ontouchstart ev _ =
    startpos := cl ev;
    currentpos := !startpos;
    previouspos := !startpos;
    previoustimestamp := now ();
    startsize := get_size ~side elt';
    let a = touchmoves elt' ontouchmove in
    let b = touchend elt' >>= ontouchend in
    let c = touchcancel elt' >>= ontouchcancel in
    let%lwt () = disable_transition elt in
    (Js.Unsafe.coerce elt'##.style)##.transitionDuration := defaultduration;
    Lwt.pick [a; b; c]
  in
  Lwt.async (fun () -> touchstarts handle' ontouchstart);
  match update with
  | Some update ->
      ignore (React.E.map (fun stop -> set 0.0 (stop, true)) update)
  | None -> ()

let%shared tongue ?(a = []) ?(side = `Bottom)
    ?(stops : stop list =
      [`Px 70; `Percent 100; `Interval (`Percent 100, `Full_content)])
    ?(init : simple_stop = `Px 70) ?handle ?update content
  =
  let a = (a :> Html_types.div_attrib attrib list) in
  let class_ = class_of_side side in
  let elt = D.div ~a:(a_class ["ot-tongue"; class_] :: a) content in
  let handle = match handle with None -> elt | Some h -> h in
  let before_signal =
    [%client
      (React.S.create ~%init
        : simple_stop React.S.t * (?step:React.step -> simple_stop -> unit))]
  in
  let after_signal =
    [%client
      (React.S.create ~%init
        : simple_stop React.S.t * (?step:React.step -> simple_stop -> unit))]
  in
  ignore
    [%client
      (Lwt.async (fun () ->
           let%lwt () = Ot_nodeready.nodeready (To_dom.of_element ~%elt) in
           bind ~%side ~%stops ~%init ~%handle
             ~%(update : simple_stop React.E.t Eliom_client_value.t option)
             (snd ~%before_signal) (snd ~%after_signal) ~%elt;
           Lwt.return_unit)
        : unit)];
  let px_signal_before =
    [%client
      (let vert = ~%side = `Top || ~%side = `Bottom in
       React.S.map (px_of_simple_stop vert ~%elt) (fst ~%before_signal)
        : int React.S.t)]
  in
  let px_signal_after =
    [%client
      (let vert = ~%side = `Top || ~%side = `Bottom in
       React.S.map (px_of_simple_stop vert ~%elt) (fst ~%after_signal)
        : int React.S.t)]
  in
  { elt
  ; stop_signal_before = [%client (fst ~%before_signal : simple_stop React.S.t)]
  ; stop_signal_after = [%client (fst ~%after_signal : simple_stop React.S.t)]
  ; px_signal_before
  ; px_signal_after }
