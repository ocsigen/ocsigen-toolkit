(** Swiping an element *)

[%%shared open Eliom_content.Html ]
[%%shared open Eliom_content.Html.F ]

let%client px_of_int v = Js.string (string_of_int v^"px")

let%client identifier ev =
  Js.Optdef.case ev##.changedTouches##(item (0))
    (fun () -> 0)
    (fun a -> a##.identifier)

let%client clX ev =
  Js.Optdef.case ev##.changedTouches##(item (0))
    (fun () -> 0)
    (fun a -> a##.clientX)

let%client clY ev =
  Js.Optdef.case ev##.changedTouches##(item (0))
    (fun () -> 0)
    (fun a -> a##.clientY)

let%client add_transition transition_duration =
  let s = Js.string (Printf.sprintf "%.2fs" transition_duration) in
  fun elt -> (Js.Unsafe.coerce (elt##.style))##.transitionDuration := s

let%client remove_transition elt =
  (Js.Unsafe.coerce (elt##.style))##.transitionDuration := Js.string "0s"

let%client dispatch_event ~ev elt name x y =
  Js.Opt.iter
    elt##.parentNode
    (fun target ->
       let event = Js.Unsafe.global##.TouchEvent in
       let touch = Js.Unsafe.global##.Touch in
       let touch = new%js touch
         (object%js
           val identifier = identifier ev
           val target = target
           val clientX = x
           val clientY = y
         end)
       in
       let opt = object%js
         val bubbles = Js._true
         val changedTouches = Js.array [| touch |]
       end
       in
       let startevent = new%js event (Js.string name) opt in
       (Js.Unsafe.coerce target)##dispatchEvent startevent
    )

let%shared bind
    ?(transition_duration = 0.3)
    ?(min : int option)
    ?(max : int option)
    ~(compute_final_pos : (int -> int) Eliom_client_value.t)
    (elt : _ elt) =
  ignore [%client
    (let elt = ~%elt in
     let elt' = To_dom.of_element elt in
     let start = ref 0 (* position when touch starts *) in
     let initial_left = ref 0 (* position before touch starts *) in
     let status = ref `Stopped in
     let onpanend ev aa =
       status := `Stopped;
       add_transition ~%transition_duration elt';
       let left = ~%compute_final_pos (clX ev - !start) in
       elt'##.style##.left := px_of_int left;
       initial_left := left;
       Lwt.async (fun () ->
         let%lwt () = Lwt_js_events.transitionend elt' in
         Manip.Class.remove elt "swiping";
         Lwt.return ());
       Lwt.return ()
     in
     let onpanstart0 () =
       status := `In_progress;
       Manip.Class.add elt "swiping"
     in
     let onpanstart ev _ =
       Dom_html.stopPropagation ev;
       remove_transition elt';
       start := (clX ev) - !initial_left;
       onpanstart0 ();
       Lwt.return ()
     in
     let onpan ev aa =
       let left = clX ev - !start in
       let do_pan () =
         elt'##.style##.left := px_of_int left;
         Dom_html.stopPropagation ev;
         Lwt.return ()
       in
       if !status = `In_progress
       then
         match ~%min, ~%max with
         | Some min, _ when left < min ->
           (* min reached.
              We stop the movement of this element
              and dispatch it to the parent. *)
           status := `Below;
           (* We send a touchstart event to the parent *)
           dispatch_event ~ev elt' "touchstart" (min + !start) (clY ev);
           (* We propagate *)
           Lwt.return ()
         | _, Some max when left > max ->
           (* max reached.
              We stop the movement of this element
              and dispatch it to the parent. *)
           status := `Above;
           (* We send a touchstart event to the parent *)
           dispatch_event ~ev elt' "touchstart" (max + !start) (clY ev);
           (* We propagate *)
           Lwt.return ()
         | _ -> do_pan ()
       else begin (* Shall we restart swiping this element? *)
         let restart_pos = match !status, ~%min, ~%max with
           | `Below, Some min, _ when left >= min -> Some min
           | `Above, _, Some max when left <= max -> Some max
           | _ -> None
         in
         match restart_pos with
         | Some restart_pos ->
           (* We restart the movement of this element *)
           (* We send a touchmove event to the parent to fix
              its position precisely,
              but no touchend because it would possibly trigger a transition. *)
           dispatch_event ~ev elt' "touchmove" (restart_pos + !start) (clY ev);
           onpanstart0 ((* restart_pos + !start *));
           do_pan ()
         | None -> (* We propagate *) Lwt.return ()
       end
     in
     Lwt.async (fun () -> Lwt_js_events.touchstarts elt' onpanstart);
     Lwt.async (fun () -> Lwt_js_events.touchmoves elt' onpan);
     Lwt.async (fun () -> Lwt_js_events.touchends elt' onpanend);
     : unit)
  ]
