(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015-09
 *      Vincent Balat
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

open%shared Eliom_content.Html
open%shared Eliom_content.Html.F
open%shared Js_of_ocaml

[%%client
type status =
  | Stopped
  | Start
  | Aborted
  | In_progress
]

let%client clX ev =
  Js.Optdef.case ev##.changedTouches##(item (0))
    (fun () -> 0)
    (fun a -> a##.clientX)

let%client clY ev =
  Js.Optdef.case ev##.changedTouches##(item (0))
    (fun () -> 0)
    (fun a -> a##.clientY)

let%client bind_click_outside bckgrnd elt close =
  Lwt.async (fun () ->
    let%lwt ev =
      Ot_lib.click_outside
        ~use_capture:true
        ~inside:(To_dom.of_element bckgrnd)
        (To_dom.of_element elt)
    in
    Dom_html.stopPropagation ev;
    close ();
    Lwt.return_unit)

let%client html () = Js.Opt.to_option @@
  Js.Opt.map (Dom_html.CoerceTo.html Dom_html.document##.documentElement)
    Of_dom.of_html
let%client html_ManipClass_add cl = match html () with
  | Some html -> Manip.Class.add html cl
  | None -> ()
let%client html_ManipClass_remove cl = match html () with
  | Some html -> Manip.Class.remove html cl
  | None -> ()

let%client add_class elt str =
  Manip.Class.add elt ("ot-dr-" ^ str);
  html_ManipClass_add @@ "ot-drawer-" ^ str
let%client remove_class elt str =
  Manip.Class.remove elt ("ot-dr-" ^ str);
  html_ManipClass_remove @@ "ot-drawer-" ^ str

(* Returns [(drawer, open_drawer, close_drawer)]
 * [ drawer ] DOM element
 * [ open_drawer ] function to open the drawer
 * [ close_drawer ] function to close the drawer *)
let%shared drawer
    ?(a = [])
    ?(position = `Left)
    ?(opened = false)
    ?(swipe = true)
    ?(onclose : (unit -> unit) Eliom_client_value.t option)
    ?(onopen : (unit -> unit) Eliom_client_value.t option)
    ?(wrap_close = fun f -> f)
    ?(wrap_open = fun f -> f)
    content =

  let scroll_pos = ref 0 in

  let a = (a :> Html_types.div_attrib attrib list) in
  let toggle_button =
    D.Form.button_no_value
      ~button_type:`Button ~a:[a_class ["ot-dr-toggle-button"]]
      []
  in
  let d = D.div ~a:[a_class [ "ot-drawer"
                            ; match position with
                            | `Top -> "ot-dr-top"
                            | `Right -> "ot-dr-right"
                            | `Bottom -> "ot-dr-bottom"
                            | `Left -> "ot-dr-left"]]
      (toggle_button :: (content :> Html_types.div_content elt list))
  in
  let bckgrnd_init_class = if opened then ["ot-dr-open"] else [] in
  let bckgrnd =
    D.div ~a:(a_class ("ot-drawer-bckgrnd" :: bckgrnd_init_class) :: a) [d]
  in

  let bind_touch :
    ((unit -> unit) Lwt.t * (unit -> unit) Lwt.u) Eliom_client_value.t =
    [%client Lwt.wait () ] in

  let touch_thread = [%client (ref (Lwt.return_unit) : unit Lwt.t ref)] in

  let reset_scroll_pos = [%client (fun () ->
     Dom_html.document##.body##.style##.top := Js.string "";
     Dom_html.window##scroll 0 !(~%scroll_pos)
  : unit -> unit)] in

  let stop_open_event = [%client (
    React.E.create () : unit React.E.t * (?step:React.step -> unit -> unit)
  )] in
  let stop_open = [%client (
    snd ~%stop_open_event : ?step:React.step -> unit -> unit
  )] in

  let close = [%client
    (fun () ->
       ~%stop_open ();
       remove_class ~%bckgrnd "open";
       ~%reset_scroll_pos ();
       add_class ~%bckgrnd "closing";
       Lwt.cancel !(~%touch_thread);
       Lwt_js_events.async (fun () ->
         let%lwt () = Lwt_js_events.transitionend (To_dom.of_element ~%d) in
         remove_class ~%bckgrnd "closing";
         Eliom_lib.Option.iter (fun f -> f ()) ~%onclose;
         Lwt.return_unit)
     : unit -> unit)]
  in
  let close = wrap_close close in

  let open_ = [%client
    ((fun () ->
       ~%scroll_pos := (Js.Unsafe.coerce Dom_html.window)##.pageYOffset;
       add_class ~%bckgrnd "open";
       Eliom_lib.Option.iter (fun f -> f ()) ~%onopen;
       Dom_html.document##.body##.style##.top :=
         Js.string (Printf.sprintf "%dpx" (- !(~%scroll_pos)));
       add_class ~%bckgrnd "opening";
       Lwt.cancel !(~%touch_thread);
       Lwt.async (fun () ->
         let%lwt bind_touch = fst ~%bind_touch in bind_touch ();
         Lwt.return_unit
       );
       bind_click_outside ~%bckgrnd ~%d ~%close;
       Eliom_client.Page_status.onactive ~stop:(fst ~%stop_open_event)
         (fun () -> html_ManipClass_add "ot-drawer-open");
       Lwt_js_events.async (fun () ->
         let%lwt () = Lwt_js_events.transitionend (To_dom.of_element ~%d) in
         remove_class ~%bckgrnd "opening";
         Lwt.return_unit)
    ) : unit -> unit)]
  in
  let open_ = wrap_open open_ in

  let _ = [%client (
    Eliom_client.Page_status.oninactive (fun () ->
      ~%reset_scroll_pos ();
      html_ManipClass_remove "ot-drawer-opening";
      html_ManipClass_remove "ot-drawer-open";
      html_ManipClass_remove "ot-drawer-closing"
    );
    Lwt.return_unit
  :unit Lwt.t)] in

  let _ = [%client
    (let toggle () =
       if Manip.Class.contain ~%bckgrnd "ot-dr-open"
       then ~%close ()
       else ~%open_ ()
     in
     Lwt_js_events.async (fun () ->
       Lwt_js_events.clicks (To_dom.of_element ~%toggle_button)
         (fun ev _ ->
            Dom.preventDefault ev ;
            Dom_html.stopPropagation ev ;
            toggle ();
            Lwt.return_unit ) )
  : unit)]
  in

  let _ = if swipe then [%client (
    (* Swipe to close: *)
    let dr = To_dom.of_element ~%d in
    let bckgrnd' = To_dom.of_element ~%bckgrnd in
    let cl = ~%close in
    let animation_frame_requested = ref false in
    let action = ref (`Move 0) in
    let perform_animation a =
      if !action = `Close && a = `Open
      then (* We received a panend after a swipeleft. We ignore it. *)
        Lwt.return_unit
      else begin
        action := a;
        if not !animation_frame_requested
        then begin
          animation_frame_requested := true;
          let%lwt () = Lwt_js_events.request_animation_frame () in
          animation_frame_requested := false;
          (match !action with
           | `Move delta ->
             let s =
               (match ~%position with
                | `Top -> "translateY(calc(100% + "
                | `Right -> "translateX(calc(-100% + "
                | `Bottom -> "translateY(calc(-100% + "
                | `Left -> "translateX(calc(100% + ")
               |> (fun t -> Printf.sprintf "%s%dpx" t delta)
               |> Js.string
             in
             (Js.Unsafe.coerce (dr##.style))##.transform := s;
             (Js.Unsafe.coerce (dr##.style))##.webkitTransform := s
           | `Close ->
             (Js.Unsafe.coerce (dr##.style))##.transform := Js.string "";
             (Js.Unsafe.coerce (dr##.style))##.webkitTransform := Js.string "";
             Lwt.async (fun () ->
               let%lwt () = Lwt_js_events.transitionend dr in
               Manip.Class.remove ~%bckgrnd "ot-swiping";
               Lwt.return_unit);
             cl ()
           | `Open ->
             (Js.Unsafe.coerce (dr##.style))##.transform := Js.string "";
             (Js.Unsafe.coerce (dr##.style))##.webkitTransform := Js.string "";
             Lwt.async (fun () ->
               let%lwt () = Lwt_js_events.transitionend dr in
               Manip.Class.remove ~%bckgrnd "ot-swiping";
               Lwt.return_unit);
           | `Abort ->
             (Js.Unsafe.coerce (dr##.style))##.transform := Js.string "";
             (Js.Unsafe.coerce (dr##.style))##.webkitTransform := Js.string "";
             Manip.Class.remove ~%bckgrnd "ot-swiping"
          );
          Lwt.return_unit
        end
        else Lwt.return_unit
      end
    in
    (* let hammer = Hammer.make_hammer bckgrnd in *)
    let startx = ref 0 (* position when touch starts *) in
    let starty = ref 0 (* position when touch starts *) in
    let status = ref Stopped in
    let onpan ev _ =
      let left = clX ev - !startx in
      let top = clY ev - !starty in
      if !status = Start
      then
        status :=
          if ((~%position = `Top || ~%position = `Bottom)
              && abs left > abs top)
          || ((~%position = `Left || ~%position = `Right)
              && abs top > abs left)
          then Aborted (* Orthogonal scrolling *)
          else if ((~%position = `Top || ~%position = `Bottom)
                   && abs top <= Ot_swipe.threshold)
               || ((~%position = `Left || ~%position = `Right) &&
                   abs left <= Ot_swipe.threshold)
          then !status
          else begin (* We decide to take the event *)
            Manip.Class.add ~%bckgrnd "ot-swiping";
            (Js.Unsafe.coerce (dr##.style))##.transition :=
              Js.string "-webkit-transform 0s, transform 0s";
            In_progress
          end;
      if !status = In_progress
      then begin
        Dom.preventDefault ev;
        Dom_html.stopPropagation ev;
        let move = ref 0 in
        if (~%position = `Top && top <= 0 && (move := top; true))
        || (~%position = `Right && left >= 0 && (move := left; true))
        || (~%position = `Bottom && top >= 0 && (move := top; true))
        || (~%position = `Left && left <= 0 && (move := left; true))
        then perform_animation (`Move !move)
        else Lwt.return_unit
      end
      else Lwt.return_unit
    in
    let onpanend ev _ =
      if !status = In_progress
      then begin
        status := Stopped;
        (Js.Unsafe.coerce (dr##.style))##.transition :=
          Js.string "-webkit-transform .35s, transform .35s";
        let width = dr##.offsetWidth in
        let deltaX = float_of_int (clX ev - !startx) in
        let deltaY = float_of_int (clY ev - !starty) in
        if (~%position = `Top && deltaY < -0.3 *. float width)
        || (~%position = `Right && deltaX > 0.3 *. float width)
        || (~%position = `Bottom && deltaY > 0.3 *. float width)
        || (~%position = `Left && deltaX < -0.3 *. float width)
        then perform_animation `Close
        else if (~%position = `Top && deltaY >= 0.)
             || (~%position = `Right && deltaX <= 0.)
             || (~%position = `Bottom && deltaY <= 0.)
             || (~%position = `Left && deltaX >= 0.)
        then perform_animation `Abort
        else perform_animation `Open
      end
      else begin
        status := Stopped;
        Lwt.return_unit
      end
    in
    let onpanstart ev _ =
      status := Start;
      startx := clX ev;
      starty := clY ev;
      let%lwt () = onpan ev a in
      (* Lwt.pick and Lwt_js_events.touch*** seem to behave oddly.
           This wrapping is an attempt to understand why. *)
      let a =
        try%lwt
              Lwt_js_events.touchmoves bckgrnd' onpan
        with Lwt.Canceled -> Lwt.return_unit
           | e ->
             let s = Printexc.to_string e in
             Printf.printf "Ot_drawer>touchmoves>exception: %s\n%!" s;
             Lwt.fail e
      and b =
        try%lwt
              (let%lwt ev = Lwt_js_events.touchend bckgrnd' in
               onpanend ev ())
        with Lwt.Canceled -> Lwt.return_unit
           | e ->
             let s = Printexc.to_string e in
             Printf.printf "Ot_drawer>touchend>exception: %s\n%!" s;
             Lwt.fail e
      and c =
        try%lwt
              (let%lwt ev = Lwt_js_events.touchcancel bckgrnd' in
               onpanend ev ())
        with Lwt.Canceled -> Lwt.return_unit
           | e ->
             let s = Printexc.to_string e in
             Printf.printf "Ot_drawer>touchcancel>exception: %s\n%!" s;
             Lwt.fail e
      in
      Lwt.pick [ a; b; c ]
    in
    Lwt.wakeup (snd ~%bind_touch) (fun () ->
      let t = Lwt_js_events.touchstarts bckgrnd' onpanstart in
      ~%touch_thread := t);
    (* Hammer.bind_callback hammer "panstart" onpanstart; *)
    (* Hammer.bind_callback hammer "panmove" onpan; *)
    (* Hammer.bind_callback hammer "panend" onpanend; *)
    (* Hammer.bind_callback hammer *)
    (*   (if ~%position = `Left then "swipeleft" else "swiperight") *)
    (*   (fun _ -> Lwt.async (fun () -> perform_animation `Close)) *)
  : unit)]
  else [%client ()]
  in

  bckgrnd, open_, close
