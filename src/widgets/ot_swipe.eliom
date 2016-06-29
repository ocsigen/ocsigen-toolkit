(** Swiping an element *)

[%%shared open Eliom_content.Html ]
[%%shared open Eliom_content.Html.F ]

(** sensibility for detecting swipe left/right or up/down *)
let%client threshold = 5

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
       let event = try
           (* Better version, but unsupported on iOS and Android: *)
           let touchevent = Js.Unsafe.global##.TouchEvent in
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
           new%js touchevent (Js.string name) opt
         with e ->
           Printf.eprintf "%s\n"
             ("exn: "^Printexc.to_string e^" - switching to workaround. ");
           (* HACK *)
           let customEvent = Js.Unsafe.global##.CustomEvent in
           let opt = object%js
             val bubbles = Js._true
           end
           in
           let event = new%js customEvent (Js.string name) opt in
           let touch = object%js
             val identifier = identifier ev
             val target = target
             val clientX = x
             val clientY = y
           end
           in
           let touches = object%js
             val item = Js.wrap_callback (fun _ -> Js.def touch)
           end
           in
           (Js.Unsafe.coerce event)##.changedTouches := touches;
           (* END HACK *)
           event
       in
       (Js.Unsafe.coerce target)##dispatchEvent event
    )

let%shared bind
    ?(transition_duration = 0.3)
    ?(min : int option)
    ?(max : int option)
    ~(compute_final_pos : (int -> int) Eliom_client_value.t)
    ?onstart
    ?onend
    (elt : _ elt) =
  ignore [%client
    (let elt = ~%elt in
     let elt' = To_dom.of_element elt in
     let startx = ref 0 (* position when touch starts *) in
     let starty = ref 0 (* position when touch starts *) in
     let status = ref `Stopped in
     let onpanend ev aa =
       if !status <> `Aborted
       then begin
       add_transition ~%transition_duration elt';
       let left = ~%compute_final_pos (clX ev - !startx) in
       elt'##.style##.left := px_of_int left;
       Lwt.async (fun () ->
         let%lwt () = Lwt_js_events.transitionend elt' in
         Manip.Class.remove elt "swiping";
         Lwt.return ());
       end;
       Eliom_lib.Option.iter (fun f -> f ()) ~%onend;
       status := `Stopped;
       Lwt.return ()
     in
     let onpanstart0 () =
       status := `Start;
     in
     let onpanstart ev _ =
       startx := clX ev - elt'##.offsetLeft;
       starty := clY ev;
       onpanstart0 ();
       Lwt.return ()
     in
     let onpan ev aa =
       let left = clX ev - !startx in
       let do_pan left = elt'##.style##.left := px_of_int left in
       if !status = `Start
       then begin
         status := if abs (clY ev - !starty) >= threshold
           then `Aborted (* vertical scrolling *)
           else if abs left >= threshold
           then begin (* We decide to take the event *)
             Manip.Class.add elt "swiping";
             remove_transition elt';
             Eliom_lib.Option.iter (fun f -> f ()) ~%onstart;
             (* We send a touchcancel to the parent (who received the start) *)
             dispatch_event ~ev elt' "touchcancel" (clX ev) (clY ev);
             `In_progress
           end
           else !status;
       end;
       if !status = `In_progress
       then
         match ~%min, ~%max with
         | Some min, _ when left < min ->
           (* min reached.
              We stop the movement of this element
              and dispatch it to the parent. *)
           status := `Below;
           do_pan min;
           (* We send a touchstart event to the parent *)
           dispatch_event ~ev elt' "touchstart" (min + !startx) (clY ev);
           (* We propagate *)
           Lwt.return ()
         | _, Some max when left > max ->
           (* max reached.
              We stop the movement of this element
              and dispatch it to the parent. *)
           status := `Above;
           do_pan max;
           (* We send a touchstart event to the parent *)
           dispatch_event ~ev elt' "touchstart" (max + !startx) (clY ev);
           (* We propagate *)
           Lwt.return ()
         | _ ->
           Dom_html.stopPropagation ev;
           Dom.preventDefault ev;
           do_pan left;
           Lwt.return ()
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
           dispatch_event ~ev elt' "touchmove" (restart_pos + !startx) (clY ev);
           onpanstart0 ((* restart_pos + !startx *));
           Dom_html.stopPropagation ev;
           do_pan left;
           Lwt.return ()
         | None -> (* We propagate *) Lwt.return ()
       end
     in
     Lwt.async (fun () -> Lwt_js_events.touchstarts elt' onpanstart);
     Lwt.async (fun () -> Lwt_js_events.touchmoves elt' onpan);
     Lwt.async (fun () -> Lwt_js_events.touchends elt' onpanend);
     Lwt.async (fun () -> Lwt_js_events.touchcancels elt' onpanend);
     : unit)
  ]
