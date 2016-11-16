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

[%%shared
  open Eliom_content.Html
  open Eliom_content.Html.F
]

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
    Lwt.return ())

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

let%client scroll_pos = ref 0

(* Returns [(drawer, open_drawer, close_drawer)]
 * [ drawer ] DOM element
 * [ open_drawer ] function to open the drawer
 * [ close_drawer ] function to close the drawer *)
let%shared drawer
    ?(a = [])
    ?(position = `Left)
    ?(opened = false)
    ?(swipe = true)
    ?(ios_scroll_pos_fix=true)
    ?(onclose : (unit -> unit) Eliom_client_value.t option)
    ?(onopen : (unit -> unit) Eliom_client_value.t option)
    ?(wrap_close = fun f -> f)
    ?(wrap_open = fun f -> f)
    content =
  let a = (a :> Html_types.div_attrib attrib list) in
  let toggle_button =
    D.Form.button_no_value
      ~button_type:`Button ~a:[a_class ["ot-dr-toggle-button"]]
      []
  in
  let d = D.div ~a:[a_class [ "ot-drawer"
                            ; match position with
                            | `Left -> "ot-dr-left"
                            | `Right -> "ot-dr-right"]]
      (toggle_button :: (content :> Html_types.div_content elt list))
  in
  let bckgrnd_init_class = if opened then ["ot-dr-open"] else [] in
  let bckgrnd =
    D.div ~a:(a_class ("ot-drawer-bckgrnd" :: bckgrnd_init_class) :: a) [d]
  in

  let bind_touch :
    ((unit -> unit) Lwt.t * (unit -> unit) Lwt.u) Eliom_client_value.t =
    [%client Lwt.wait () ] in

  let touch_thread = [%client (ref (Lwt.return ()) : unit Lwt.t ref)] in

  let close = [%client
    ((fun () ->
       remove_class ~%bckgrnd "open";
       if ~%ios_scroll_pos_fix then
         Dom_html.document##.body##.scrollTop := !scroll_pos;
       add_class ~%bckgrnd "closing";
       Lwt.cancel !(~%touch_thread);
       Lwt_js_events.async (fun () ->
         let%lwt () = Lwt_js_events.transitionend (To_dom.of_element ~%d) in
         remove_class ~%bckgrnd "closing";
         Eliom_lib.Option.iter (fun f -> f ()) ~%onclose;
         Lwt.return ()))
     : unit -> unit)]
  in
  let close = wrap_close close in

  let open_ = [%client
    ((fun () ->
       if ~%ios_scroll_pos_fix then
         scroll_pos := Dom_html.document##.body##.scrollTop;
       add_class ~%bckgrnd "open";
       Eliom_lib.Option.iter (fun f -> f ()) ~%onopen;
       if ~%ios_scroll_pos_fix then
         Dom_html.document##.body##.scrollTop := !scroll_pos;
       add_class ~%bckgrnd "opening";
       Lwt.cancel !(~%touch_thread);
       Lwt.async (fun () ->
         let%lwt bind_touch = fst ~%bind_touch in bind_touch ();
         Lwt.return_unit
       );
       bind_click_outside ~%bckgrnd ~%d ~%close;
       Lwt_js_events.async (fun () ->
         let%lwt () = Lwt_js_events.transitionend (To_dom.of_element ~%d) in
         remove_class ~%bckgrnd "opening";
         Lwt.return ()))
     : unit -> unit)]
  in
  let open_ = wrap_open open_ in

  let _ = [%client (
    let%lwt () = Ot_nodeready.nodeready (To_dom.of_element ~%d) in
    (Eliom_client.onunload @@ fun () ->
    html_ManipClass_remove "ot-drawer-opening";
    html_ManipClass_remove "ot-drawer-open";
    html_ManipClass_remove "ot-drawer-closing");
    Lwt.return ()
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
            Lwt.return () ) )
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
        Lwt.return ()
      else begin
        action := a;
        if not !animation_frame_requested
        then begin
          animation_frame_requested := true;
          let%lwt () = Lwt_js_events.request_animation_frame () in
          animation_frame_requested := false;
          (match !action with
           | `Move delta ->
             let s = Js.string ((if ~%position = `Right
                                 then "translateX(calc(-100% + "
                                 else "translateX(calc(100% + ")^
                                string_of_int delta^"px))") in
             (Js.Unsafe.coerce (dr##.style))##.transform := s;
             (Js.Unsafe.coerce (dr##.style))##.webkitTransform := s
           | `Close ->
             (Js.Unsafe.coerce (dr##.style))##.transform := Js.string "";
             (Js.Unsafe.coerce (dr##.style))##.webkitTransform := Js.string "";
             Lwt.async (fun () ->
               let%lwt () = Lwt_js_events.transitionend dr in
               Manip.Class.remove ~%bckgrnd "ot-swiping";
               Lwt.return ());
             cl ()
           | `Open ->
             (Js.Unsafe.coerce (dr##.style))##.transform := Js.string "";
             (Js.Unsafe.coerce (dr##.style))##.webkitTransform := Js.string "";
             Lwt.async (fun () ->
               let%lwt () = Lwt_js_events.transitionend dr in
               Manip.Class.remove ~%bckgrnd "ot-swiping";
               Lwt.return ());
          );
          Lwt.return ()
        end
        else Lwt.return ()
      end
    in
    (* let hammer = Hammer.make_hammer bckgrnd in *)
    let startx = ref 0 (* position when touch starts *) in
    let starty = ref 0 (* position when touch starts *) in
    let status = ref Stopped in
    let onpan ev _ =
      let left = clX ev - !startx in
      if !status = Start
      then begin
        status := if abs (clY ev - !starty) > abs left
          then Aborted (* vertical scrolling *)
          else if abs left > Ot_swipe.threshold
          then begin (* We decide to take the event *)
            Manip.Class.add ~%bckgrnd "ot-swiping";
            (Js.Unsafe.coerce (dr##.style))##.transition :=
              Js.string "-webkit-transform 0s, transform 0s";
            In_progress
          end
          else !status
      end;
      if !status = In_progress
      then begin
        Dom.preventDefault ev;
        Dom_html.stopPropagation ev;
        if (~%position = `Left && left <= 0)
        || (~%position = `Right && left >= 0)
        then perform_animation (`Move left)
        else Lwt.return ()
      end
      else Lwt.return ()
    in
    let onpanend ev _ =
      if !status <>  Start
      then begin
        status := Stopped;
        (Js.Unsafe.coerce (dr##.style))##.transition :=
          Js.string "-webkit-transform .35s, transform .35s";
        let width = dr##.offsetWidth in
        let delta = float_of_int (clX ev - !startx) in
        if (~%position = `Left && delta < -0.3 *. float width)
        || (~%position = `Right && delta > 0.3 *. float width)
        then perform_animation `Close
        else perform_animation `Open
      end
      else begin
        status := Stopped;
        Lwt.return ()
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
