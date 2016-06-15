(** Swiping an element *)

[%%shared open Eliom_content.Html ]
[%%shared open Eliom_content.Html.F ]

let%client px_of_int v = Js.string (string_of_int v^"px")

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

let%client onpan start elt' ev _ =
  elt'##.style##.left := px_of_int (clX ev - !start);
  Dom_html.stopPropagation ev;
  Lwt.return ()

let%shared bind
    ?(transition_duration = 0.3)
    ~compute_final_pos
    elt =
  ignore [%client
    (let elt = ~%elt in
     let elt' = To_dom.of_element elt in
     let start = ref 0 in
     Lwt.async (fun () -> Lwt_js_events.touchstarts elt' (fun ev _ ->
       start := clX ev - !start;
       Dom_html.stopPropagation ev;
       Manip.Class.add elt "swiping";
       Lwt.return ()
     ));
     Lwt.async (fun () -> Lwt_js_events.touchmoves elt' (onpan start elt'));
     Lwt.async (fun () -> Lwt_js_events.touchends elt' (fun ev _ ->
       Dom_html.stopPropagation ev;
       add_transition ~%transition_duration elt';
       let left = ~%compute_final_pos (clX ev - !start) in
       elt'##.style##.left := px_of_int left;
       start := left; (* when not swiping,
                         start contains the left position of the block *)
      Lwt.async (fun () ->
        let%lwt () = Lwt_js_events.transitionend elt' in
        remove_transition elt';
        Manip.Class.remove elt "swiping";
        Lwt.return ());
       Lwt.return ()));
     : unit)
  ]
