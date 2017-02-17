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

(*

TODO:
 - Make it possible to change the number of pages
 - Circular carousel (swipe to first page after last page)
 - Wheel: do not take face_size as parameter but compute it
   (what if generated server side?)

*)


[%%shared
open Eliom_content.Html
open Eliom_content.Html.F
]

let%client clX = Ot_swipe.clX

let%client clY = Ot_swipe.clY

let%client add_transition transition_duration =
  let s = Js.string (Printf.sprintf "%.2fs" transition_duration) in
  fun elt -> (Js.Unsafe.coerce (elt##.style))##.transitionDuration := s

let%client remove_transition elt =
  (Js.Unsafe.coerce (elt##.style))##.transitionDuration := Js.string "0s"

let%shared default_make_transform ~vertical ?(delta = 0) pos =
  let p = -pos * 100 in
  let sign = if delta > 0 then "+" else "-" in
  let d = abs delta in
  if vertical
  (* then Printf.sprintf "translate3d(0, %.3f%%, 0)" d *)
  (* else Printf.sprintf "translate3d(%.3f%%, 0, 0)" d *)
  then Printf.sprintf "translate(0, calc(%d%% %s %dpx)" p sign d
  else Printf.sprintf "translate(calc(%d%% %s %dpx), 0)" p sign d
(* translate3d possibly more efficient on some devices ... *)
(* But causing troubles ...
   For example some content cannot have border-radius on Chrome ... *)

let%shared ot_swiping = "ot-swiping"

[%%client
let now () = (new%js Js.date_now)##getTime /. 1000.

let average_time = 0.1 (* the time, in seconds,
                          for computing the moving average
                          (the current speed is the average speed
                          on that period) *)

type status =
  | Stopped
  | Start of (int * int * float) (* Just started, x, y positions, timestamp *)
  | Ongoing of (int * int * int * float * int * float)
    (* Ongoing swipe, (x start position,
                       y start position,
                       element width,
                       current speed average over average_time,
                       previous position,
                       previous timestamp)
    *)
]

let%shared make
    ?(a = [])
    ?(vertical = false)
    ?(position = 0)
    ?(transition_duration = 0.6)
    ?(inertia = 1.0)
    ?(swipeable = true)
    ?(allow_overswipe = false)
    ?(update : [`Goto of int | `Next | `Prev ] React.event Eliom_client_value.t option)
    ?(disabled = Eliom_shared.React.S.const false)
    ?(full_height = `No)
    ?(make_transform = [%shared default_make_transform])
    ?(make_page_attribute = [%shared fun ~vertical:_ _ -> [] ])
    l =
  let a = (a :> Html_types.div_attrib attrib list) in
  let pos_signal, pos_set = Eliom_shared.React.S.create position in
  let swipe_pos_sig = [%client
    (React.S.create 0.
     : (float React.S.t * (?step:React.step -> float -> unit))) ]
  in
  let swipe_pos = [%client (fst ~%swipe_pos_sig : float React.S.t) ] in
  let swipe_pos_set =
    [%client (snd ~%swipe_pos_sig : ?step:React.step -> float -> unit) ]
  in
  (* We wrap all pages in a div in order to add class carpage,
     for efficiency reasons in CSS (avoids selector ".ot-car2>*")*)
  let pages = List.mapi (fun i e ->
    D.div ~a:( a_class ["ot-carpage"]
               :: Eliom_shared.Value.local make_page_attribute ~vertical i)
                            [e]) l
  in
  let initial_translation =
    if position = 0
    then []
    else [ a_style
             ("transform: "^
              (Eliom_shared.Value.local make_transform) ~vertical position)
         ]
  in
  let d2 = D.div ~a:(a_class ["ot-car2"]::initial_translation) pages in
  let d = D.div
      ~a:(a_class ("ot-carousel"
                   :: (if vertical then "ot-vertical" else "ot-horizontal")
                   :: if full_height = `No then [] else ["ot-full-height"])
          ::a)
      [d2]
  in
  let maxi = List.length pages - 1 in
  let nb_visible_elements, set_nb_visible_elements =
    Eliom_shared.React.S.create 1
  in
  let _ = [%client (
    let add_transition = add_transition ~%transition_duration in
    let vertical = ~%vertical in
    let d2' = To_dom.of_element ~%d2 in
    let d = To_dom.of_element ~%d in
    (* The scroll position of each page: *)
    let scroll_pages = List.map (fun _ -> ref 0) ~%pages in
    let width_element () =
      if vertical then d2'##.offsetHeight else d2'##.offsetWidth
    in
    let comp_nb_visible_elements () =
      (* We suppose that all elements have the same width *)
      let width_element = width_element () in
      if width_element = 0
      then 1
      else
        let width_carousel =
          if vertical then d##.offsetHeight else d##.offsetWidth
        in
        truncate ((float width_carousel) /. (float width_element) +. 0.5)
    in
    Lwt.async (fun () ->
      let%lwt () = Ot_nodeready.nodeready d2' in
      ~%set_nb_visible_elements (comp_nb_visible_elements ());
      Lwt.return ());
    let maxi () = ~%maxi - (React.S.value ~%nb_visible_elements) + 1 in
    let pos_signal = ~%pos_signal in
    let pos_set = ~%pos_set in
    let action = ref (`Move (0, 0)) in
    let animation_frame_requested = ref false in
    (**********************
       setting class active on visible pages (only)
    *)
    let set_active () =
      (* Adding class "ot-active" to all visible pages *)
        List.iteri (fun i page ->
          Manip.Class.remove page "ot-active";
          let pos = React.S.value pos_signal in
          if i >= pos && i < pos + React.S.value ~%nb_visible_elements
          then (* Page is visible *) Manip.Class.add page "ot-active";
        )
          ~%pages
    in
    (********************** end *)
    (**********************
       full_height
    *)
    let set_top_margin () =
      if not vertical
      then
        let dist = match ~%full_height with
          (* In full height mode, just before swiping,
             we add margin on top of columns,
             to adapt it to the current scroll position of page.
          *)
          | `No -> None
          | `No_header -> Some 0
          | `Header (f : unit -> int) -> Some (f ())
        in
        Eliom_lib.Option.iter
          (fun dist ->
             let delta = max 0 (dist - int_of_float (Ot_size.client_top d)) in
             let pos = React.S.value pos_signal in
             Ot_lib.List.iteri2
               (fun i coli scrolli ->
                  if i <> pos
                  then
                    let coli' = To_dom.of_element coli in
                    coli'##.style##.marginTop :=
                      Js.string (string_of_int (delta - !scrolli) ^ "px")
                  else (* we save the scroll position of active column: *)
                    scrolli := delta
               )
               ~%pages scroll_pages)
          dist
    in
    let unset_top_margin () =
      (* In full height mode, after swiping,
         we remove the margins and set the correct
         scroll position for the page *)
      let dist = match ~%full_height, vertical with
        (* In full height mode, during swipe,
           we add margin on top of columns,
           to adapt it to the current scroll position of page.
        *)
        | _, true -> None
        | `No, _ -> None
        | `No_header, _ -> Some 0
        | `Header (f : unit -> int), _ -> Some (f ())
      in
      Eliom_lib.Option.iter
        (fun dist ->
           let delta = - max 0 (dist - int_of_float (Ot_size.client_top d)) in
           let pos = React.S.value pos_signal in
           Ot_lib.List.iteri2
             (fun i coli scrolli ->
                let coli' = To_dom.of_element coli in
                coli'##.style##.marginTop := Js.string "0px";
                if i = pos
                then (Js.Unsafe.coerce Dom_html.window)##scrollBy
                    0 (!scrolli + delta)
             )
             ~%pages scroll_pages)
        dist
    in
    (********************** end *)
    let set_position ?transitionend pos =
      let pos = max 0 (min pos (maxi ())) in
      let s = Js.string @@ (Eliom_shared.Value.local ~%make_transform)
          ~vertical pos
      in
      (Js.Unsafe.coerce (d2'##.style))##.transform := s;
      (Js.Unsafe.coerce (d2'##.style))##.webkitTransform := s;
      let step = React.Step.create () in
      pos_set ~step pos;
      ~%swipe_pos_set ~step 0.;
      React.Step.execute step;
      set_active ();
      Lwt.async (fun () ->
        let%lwt () = Lwt_js_events.transitionend d2' in
        Eliom_lib.Option.iter (fun f -> f ()) transitionend;
        Manip.Class.remove ~%d2 ot_swiping;
        (* Remove swiping after calling f,
           because f will possibly change the scrolling position of the page *)
        Lwt.return ())
    in
    (*VVV I recompute the size everytime we touch the carousel
        and when the window is resized (?).
        Should be: every time the carousel size or content size changes
        and/or: provide a function to recompute size *)
    let update_size =
      React.S.map
        (fun _ -> ~%set_nb_visible_elements (comp_nb_visible_elements ()))
        (if vertical then Ot_size.height else Ot_size.width)
    in
    Lwt.async (fun () ->
      let%lwt () = Ot_nodeready.nodeready d2' in
      set_position ~%position;
      add_transition d2';
      Eliom_client.onunload (fun () -> React.S.stop ~strong:true update_size);
      Lwt.return ());
    let perform_animation a =
      ~%set_nb_visible_elements (comp_nb_visible_elements ());
      if not (React.S.value ~%disabled)
      then
        match !action, a with
        | `Change _, _ ->
          (* We received both a panend and a swipe.
             The panend can be a `Goback and the swipe a `Change.
             We ignore the `Goback. *)
          Lwt.return ()
        | _ ->
          action := a;
          if not !animation_frame_requested
          then begin
            animation_frame_requested := true;
            let%lwt () = Lwt_js_events.request_animation_frame () in
            animation_frame_requested := false;
            (match !action with
             | `Move (delta, width_element) ->
               let delta = if ~%allow_overswipe
                 then delta
                 else
                   let global_delta =
                     React.S.value ~%pos_signal * width_element
                   in
                   let m = - width_element * (maxi ()) + global_delta in
                   min global_delta (max delta m)
               in
               let sign = if delta < 0 then " - " else " + " in
               let pos = Eliom_shared.React.S.value pos_signal in
               ~%swipe_pos_set (-. (float delta) /. float width_element);
               let s = ~%make_transform ~vertical ~delta pos in
               (Js.Unsafe.coerce (d2'##.style))##.transform := s;
               (Js.Unsafe.coerce (d2'##.style))##.webkitTransform := s;
             | `Goback position
             | `Change position ->
               Manip.Class.add ~%d2 ot_swiping;
               set_top_margin ();
               action := `Move (0, 0);
               set_position ~transitionend:unset_top_margin position);
            Lwt.return ()
          end
          else Lwt.return ()
      else Lwt.return ()
    in
    let status = ref Stopped in
    let compute_speed prev_speed prev_delta prev_timestamp delta =
      let timestamp = now () in
      let delta_t = timestamp -. prev_timestamp in
      let speed =
        if delta_t = 0.
        then prev_speed
        else
          let cur_speed = (float delta -. float prev_delta) /. delta_t in
          if delta_t >= average_time
          then cur_speed
          else ((average_time -. delta_t) *. prev_speed
                +. delta_t *. cur_speed)
               /. average_time
      in
      timestamp, speed
    in
    let onpan ev _ =
      (match !status with
       | Start (startx, starty, prev_timestamp) ->
         let move = if vertical then clY ev - starty else clX ev - startx in
         status :=
           if abs (if vertical
                   then clX ev - startx
                   else clY ev - starty) >= abs move
           then Stopped (* swiping in wrong direction (vertical/horizontal) *)
           else
             if abs move > Ot_swipe.threshold
             then begin
               Manip.Class.add ~%d2 ot_swiping;
               set_top_margin ();
               remove_transition d2';
               let timestamp = now () in
               let delta_t = timestamp -. prev_timestamp in
               let speed = if delta_t = 0. then 0. else float move /. delta_t in
               Ongoing (startx, starty, width_element (),
                        speed, move, timestamp)
             end
             else !status
       | _ -> ());
      (match !status with
       | Ongoing (startx, starty, width_element,
                  prev_speed, prev_delta, prev_timestamp) ->
         Dom.preventDefault ev;
         Dom_html.stopPropagation ev; (* in case there is a carousel
                                         in a carousel, e.g. *)
         let delta =
           if vertical
           then clY ev - starty
           else clX ev - startx
         in
         let timestamp, speed =
           compute_speed prev_speed prev_delta prev_timestamp delta
         in
         status :=
           Ongoing (startx, starty, width_element, speed, delta, timestamp);
         Lwt.async
           (fun () -> perform_animation (`Move (delta, width_element)))
       | _ -> ());
      Lwt.return ()
    in
    (* let hammer = Hammer.make_hammer d2 in *)
    let do_end ev startx starty prev_speed prev_delta prev_timestamp =
      Dom_html.stopPropagation ev; (* in case there is a carousel
                                      in a carousel, e.g. *)
      add_transition d2';
      status := Stopped;
      let width, delta =
        if vertical
        then d2'##.offsetHeight, (clY ev - starty)
        else d2'##.offsetWidth, (clX ev - startx)
      in
      let timestamp, speed =
        compute_speed prev_speed prev_delta prev_timestamp delta
      in
      let pos = Eliom_shared.React.S.value pos_signal in
      let delta = delta + (int_of_float
                             (speed *. ~%transition_duration *. ~%inertia) / 2)
      in
      let rem = delta mod width in
      let nbpages = - (delta / width +
                       if rem > width / 2 then 1
                       else if rem < - (width / 2) then -1
                       else 0)
      in
      let newpos = pos + nbpages in
      let newpos =
        let maxi = maxi () in
        if newpos < 0 then 0 else if newpos > maxi then maxi else newpos
      in
      if newpos <> pos
      then perform_animation (`Change newpos)
      else perform_animation (`Goback newpos)
    in
    let touchend ev _ =
      match !status with
      | Start (startx, starty, timestamp) ->
        do_end ev startx starty 0. 0 timestamp
      | Ongoing (startx, starty, _width, speed, delta, timestamp) ->
        do_end ev startx starty speed delta timestamp
      | _ -> Lwt.return ()
    in
    let touchcancel ev _ =
      match !status with
      | Start (startx, starty, _)
      | Ongoing (startx, starty, _, _, _, _) ->
        add_transition d2';
        status := Stopped;
        let pos = Eliom_shared.React.S.value pos_signal in
        perform_animation (`Goback pos)
      | _ -> Lwt.return ()
    in
    if ~%swipeable
    then begin
      Lwt.async (fun () -> Lwt_js_events.touchstarts d (fun ev aa ->
        status := Start (clX ev, clY ev, now ());
        Lwt.return ()));
      Lwt.async (fun () -> Lwt_js_events.touchmoves d onpan);
      Lwt.async (fun () -> Lwt_js_events.touchends d touchend);
      Lwt.async (fun () -> Lwt_js_events.touchcancels d touchcancel);
    end;
    ignore
      (Eliom_lib.Option.map
         (fun update ->
            React.E.map (fun v ->
              let maxi = maxi () in
              match v with
              | `Goto pos ->
                let pos =
                  if pos < 0 then 0 else if pos > maxi then maxi else pos
                in
                perform_animation (`Change pos)
              | `Next ->
                let curpos = Eliom_shared.React.S.value pos_signal in
                if curpos < maxi then perform_animation (`Change (curpos + 1))
                else Lwt.return ()
              | `Prev ->
                let curpos = Eliom_shared.React.S.value pos_signal in
                if curpos > 0 then perform_animation (`Change (curpos - 1))
                else Lwt.return ())
            update)
       ~%update);
  : unit)]
  in
  d, pos_signal, nb_visible_elements, swipe_pos

let%shared bullet_class i pos size =
  Eliom_shared.React.S.l2
    [%shared  fun p size ->
       if ~%i >= p && ~%i < p + size
       then ["ot-active"] else [] ] pos size

let%shared bullets
    ?(a = []) ?attributes
    ~(change : ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t)
    ~pos ~length
    ?(size = Eliom_shared.React.S.const 1)
    ?content () =
  let a = (a :> Html_types.ul_attrib attrib list) in
  let bullet i c =
    let class_ = bullet_class i pos size in
    let a = match attributes with
      | None -> []
      | Some l -> (List.nth l i :> Html_types.li_attrib attrib list)
    in
    li ~a:(a_class [ "ot-bullet-nav-item" ]
           :: R.a_class class_
           :: a_onclick [%client fun _ -> ~%change (`Goto ~%i)]
           :: a)
      c
  in
  let content = match content with
    | Some content when List.length content = length ->
      content
    | None ->
      let rec empty l res = if l = 0 then res else empty (l-1) ([]::res) in
      empty length []
    | _ ->
      invalid_arg "content"
  in
  ul ~a:(a_class ["ot-bullet-nav"]::a) (List.mapi bullet content)

let%shared ribbon
    ?(a = [])
    ~(change : ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t)
    ~pos
    ?(size = Eliom_shared.React.S.const 1)
    ?(initial_gap = 0)
    ?(transition_duration = 0.6)
    ?(cursor : float React.S.t Eliom_client_value.t option)
    l =
  let a = (a :> Html_types.div_attrib attrib list) in
  let item i c =
    let class_ = bullet_class i pos size in
    D.li ~a:[ a_class ["ot-car-ribbon-list-item"]
            ; R.a_class class_
            ; a_onclick  [%client fun _ -> ~%change (`Goto ~%i)]
            ]
      c
  in
  let l = List.mapi item l in
  let nb_pages = List.length l in
  let the_ul = D.ul ~a:[a_class ["ot-car-ribbon-list"]] l in
  let cursor_elt =
    Eliom_lib.Option.map (fun c -> D.div ~a:[a_class ["ot-car-cursor"]] [])
      cursor
  in
  let cursor_l = match cursor_elt with None -> [] | Some c -> [ c ] in
  let container = D.div ~a:(a_class ["ot-car-ribbon"]::a) (the_ul::cursor_l) in
  ignore [%client (
    let add_transition = add_transition ~%transition_duration in
    let the_ul = ~%the_ul in
    let nb_pages = ~%nb_pages in
    let container = ~%container in
    let container' = To_dom.of_element container in
    let initial_gap = ~%initial_gap in
    let the_ul' = To_dom.of_element the_ul in
    let cursor_elt' = Eliom_lib.Option.map To_dom.of_element ~%cursor_elt in
    let containerwidth, set_containerwidth =
      React.S.create container'##.offsetWidth in
    let curleft, set_curleft = React.S.create initial_gap in
    Lwt.async (fun () ->
      let%lwt () = Ot_nodeready.nodeready container' in
      (* Ribbon position: *)
      set_containerwidth container'##.offsetWidth ;
      add_transition the_ul';
      Eliom_lib.Option.iter add_transition cursor_elt';
      Ot_noderesize.noderesize (Ot_noderesize.attach container') (fun () ->
        set_containerwidth container'##.offsetWidth
      ) ;
      (* noderesize is not very reliable, for example if we remove the
         node from page and put it back.
         As a temporary workaround, a also update containerwidth
         when the browser window's size changes: *)
      let watch_width = Ot_size.width |> React.S.map @@ fun _ ->
        (* This delay is a hack to make the ribbon work when it is inside of a
           container that is sticky due to [Ot_sticky.make_sticky]. The problem
           is that Ot_sticky functions by moving around the contents of the
           container in the DOM when the window is resized. This destroys the
           noderesize but there is also a race condition with this code here
           that runs on window resizing. So we make sure the ribbon code runs
           AFTER it has been placed into the fixed container by Ot_sticky. *)
        Lwt.async @@ fun () ->
          let%lwt _ = Lwt_js.sleep 0.05 in
          set_containerwidth container'##.offsetWidth;
          Lwt.return ()
      in
      Eliom_client.onunload (fun () -> React.S.stop ~strong:true watch_width);
      (* Changing the position of the ribbon when the carousel position
         changes or when the size of the window changes: *)
      ignore
        (React.S.l3
           (fun pos size containerwidth ->
              let firstelt = Manip.nth the_ul 0 in
              let firstselectedelt = Manip.nth the_ul pos in
              let lastselectedelt = Manip.nth the_ul (pos + size - 1) in
              match firstelt, firstselectedelt, lastselectedelt with
              | Some _, Some firstselectedelt, Some lastselectedelt ->
                let firstselectedelt = To_dom.of_element firstselectedelt in
                let lastselectedelt = To_dom.of_element lastselectedelt in
                let left = firstselectedelt##.offsetLeft in
                let right =
                  lastselectedelt##.offsetLeft + lastselectedelt##.offsetWidth
                in
                (* We try to center the active columns *)
                let newleft = -(left + right - containerwidth) / 2 in
                (* If there is space before but not after,
                      or vice-versa,
                      we prefer balancing the whole: *)
                let ul_width = (To_dom.of_element the_ul)##.scrollWidth in
                let newleft =
                  if newleft > 0
                  then max initial_gap ((containerwidth - ul_width) / 2)
                  else if newleft + ul_width < containerwidth
                  then min
                      (containerwidth - ul_width - initial_gap)
                      ((containerwidth - ul_width) / 2)
                  else newleft
                in
                set_curleft newleft;
                the_ul'##.style##.left := Js.string (string_of_int newleft^"px")
              | _ -> ()
           ) ~%pos ~%size containerwidth);
      (* Cursor: *)
      (match ~%cursor_elt, ~%cursor with
       | Some cursor_elt, Some cursor ->
         let moving = ref false in
         ignore
           (React.S.l5
              (fun pos offset size curleft _ ->
                 if not !moving && offset <> 0.
                 then begin
                   (* Carousel is being swiped.
                      Removing transition on cursor. *)
                   moving := true;
                   Eliom_lib.Option.iter remove_transition cursor_elt';
                 end;
                 if offset = 0. && !moving
                 then begin
                   moving := false;
                   Eliom_lib.Option.iter add_transition cursor_elt';
                 end;
                 let firstselectedelt = Manip.nth the_ul pos in
                 let lastselectedelt = Manip.nth the_ul (pos + size - 1) in
                 let previouselt =
                   if pos > 0 then Manip.nth the_ul (pos - 1) else None
                 in
                 let nextelt =
                   if pos + size < nb_pages
                   then Manip.nth the_ul (pos + size)
                   else None
                 in
                 match firstselectedelt, lastselectedelt with
                 | Some firstselectedelt, Some lastselectedelt ->
                   let firstselectedelt = To_dom.of_element firstselectedelt in
                   let lastselectedelt = To_dom.of_element lastselectedelt in
                   let left = firstselectedelt##.offsetLeft + curleft in
                   let right =
                     - lastselectedelt##.offsetLeft
                     - lastselectedelt##.offsetWidth
                     + the_ul'##.offsetWidth
                     - curleft
                   in
                   let offset_left =
                     if offset >= 0.
                     then int_of_float
                         (offset *. float firstselectedelt##.offsetWidth)
                     else
                       match previouselt with
                       | None -> 0
                       | Some elt ->
                         int_of_float
                           (offset
                            *. float (To_dom.of_element elt)##.offsetWidth)
                   in
                   let offset_right =
                     if offset <= 0.
                     then
                       int_of_float
                         (offset *. float lastselectedelt##.offsetWidth)
                     else
                       match nextelt with
                       | None -> 0
                       | Some elt ->
                         int_of_float
                           (offset
                            *. float (To_dom.of_element elt)##.offsetWidth)
                   in
                   let left =
                     Js.string (string_of_int (left + offset_left) ^ "px") in
                   let right =
                     Js.string (string_of_int (right - offset_right) ^ "px") in
                   (To_dom.of_element cursor_elt)##.style##.left := left;
                   (To_dom.of_element cursor_elt)##.style##.right := right
                 | _ -> ()
              )
              ~%pos
              cursor
              ~%size
              curleft
              containerwidth
           );
       | _ -> ()
      );
      Lwt.return ()
    );
    (* Moving the ribbon with fingers: *)
    let fmax () = initial_gap in
    let fmin () =
      let ul_width = (To_dom.of_element the_ul)##.scrollWidth in
      React.S.value containerwidth - ul_width - initial_gap
    in
    Ot_swipe.bind
      ~min:fmin
      ~max:fmax
      ~compute_final_pos:(fun ev p ->
        let cw = React.S.value containerwidth in
        let pos = (max (- the_ul'##.scrollWidth + cw/2) (min (cw / 2) p)) in
        (* Limit the movement
            (make this configurable by optional parameter?): *)
        let pos = min (fmax ()) pos in
        let pos = max (fmin ()) pos in
        set_curleft pos;
        pos
      )
      ~onmove:(fun ev p ->
        Dom.preventDefault ev;
        let pos = p in
        (* Limit the movement
            (make this configurable by optional parameter?): *)
        let pos = min (fmax ()) pos in
        let pos = max (fmin ()) pos in
        set_curleft pos)
      ~onstart:(fun _ _ -> Eliom_lib.Option.iter remove_transition cursor_elt')
      ~onend:(fun ev _ -> Eliom_lib.Option.iter add_transition cursor_elt')
      the_ul;
    Lwt.return () : _)];
  container

let%shared blur = function true -> ["ot-blurred"] | false -> []

let%shared previous ?(a = [])
    ~(change : ([> `Prev | `Goto of int ] -> unit) Eliom_client_value.t)
    ?(offset = Eliom_shared.React.S.const 1)
    ~pos content =
  Form.button_no_value
    ~button_type:`Button
    ~a:(R.a_class (Eliom_shared.React.S.map [%shared fun p -> blur (p = 0)] pos)
        :: a_class ["ot-car-prev"]
        :: a_onclick  [%client
          fun _ ->
            let offset = React.S.value ~%offset in
            ~%change (if offset > 1
                      then `Goto (React.S.value ~%pos - offset)
                      else `Prev) ]
        :: (a :> Html_types.button_attrib
                Eliom_content.Html.attrib list) )
    content

let%shared next ?(a = [])
    ~(change : ([> `Next | `Goto of int ] -> unit) Eliom_client_value.t)
    ?(offset = Eliom_shared.React.S.const 1)
    ~pos ~size ~length content =
  Form.button_no_value
    ~button_type:`Button
    ~a:(R.a_class (Eliom_shared.React.S.l2
                     [%shared fun p s -> blur (p + s >= ~%length)]
                     pos size)
        :: a_class ["ot-car-next"]
        :: a_onclick  [%client
          fun _ -> let offset = React.S.value ~%offset in
            ~%change (if offset > 1
                      then `Goto (React.S.value ~%pos + offset)
                      else `Next) ]
        :: (a :> Html_types.button_attrib
                Eliom_content.Html.attrib list) )
    content

(* (\* Menu + prev/next buttons *\) *)
(* let nav ?(a = []) ~change ~pos ?size l = *)
(*   let menu = menu ~change ~pos ?size l in *)
(*   let prev = button *)
(*       ~button_type:`Button *)
(*       ~a:[a_class ["ot-car-prev"]; *)
(*           a_onclick {{ fun _ -> %change `Prev }}] *)
(*       [pcdata ([%i18n prev_col ())] *)
(*   in *)
(*   let next = button *)
(*       ~button_type:`Button *)
(*       ~a:[a_class ["ot-car-next"]; *)
(*           a_onclick {{ fun _ -> %change `Next }}] *)
(*       [pcdata ([%i18n next_col ())] *)
(*   in *)
(*   D.div ~a:(a_class ["ot-car-nav"]::a) [prev; menu; next] *)



let%client bind_arrow_keys ?use_capture ?(vertical = false) ~change elt =
  Lwt_js_events.keydowns ?use_capture elt (fun ev _ ->
    let change d =
      Dom_html.stopPropagation ev;
      Dom.preventDefault ev;
      change d
    in
    let on_text_input =
      match Dom_html.opt_tagged (ev##.target) with
      | Some (Dom_html.Textarea _) -> true
      | Some (Dom_html.Input _)    -> true
      | _ -> Js.Opt.case ev##.target
               (fun () -> false)
               (fun x ->
                  Js.to_bool (x##hasAttribute (Js.string "contenteditable")))
    in
    let key = ev##.keyCode in
    if on_text_input then () else
    if vertical
    then (if key = 40 (* down *)
          then change `Next
          else if key = 38 (* up *)
          then change `Prev)
    else (if key = 39 (* right *)
          then change `Next
          else if key = 37 (* left *)
          then change `Prev);
    Lwt.return ()
  )

let%shared wheel_compute_angle pos faces swipe_pos =
  (float pos +. swipe_pos) *. (360. /. float faces)

let%shared wheel_make_transform z faces face_size ~vertical ?(delta = 0) pos =
  let angle = wheel_compute_angle pos faces (-. float delta /. float face_size)
  in
  if vertical
  then Printf.sprintf "translateZ(%dpx) rotateX(%.3fdeg)" (-z) angle
  else Printf.sprintf "translateZ(%dpx) rotateY(%.3fdeg)" (-z) angle

let%shared wheel_page_attribute pos z faces length ~vertical page_number =
  let v = if vertical then "X" else "Y" in
  let angle = -. float page_number *. 360. /. float faces in
  let style = Printf.sprintf "transform: rotate%s(%.3fdeg) translateZ(%dpx)"
      v angle z
  in
  let cls = Eliom_shared.React.S.map
      [%shared fun (pos, swipe_pos) ->
        let faces = ~%faces in
        let page_number = ~%page_number in
        [ if float page_number <= float pos +. swipe_pos -. float faces /. 2.
          || float page_number >  float pos +. swipe_pos +. float faces /. 2.
          then "ot-hidden-wheel-face"
          else "" ]
      ]
      pos
  in
  [ R.a_class cls
  ; a_style style ]

let%shared wheel
    ?(a = [])
    ?(vertical = true)
    ?(position = 0)
    ?transition_duration
    ?inertia
    ?allow_overswipe
    ?update
    ?disabled
    ?(faces = 20)
    ?(face_size = 25)
    content =
  let a = a_class ["ot-wheel"]::a in
  let length = List.length content in
  let z = int_of_float
      (float face_size /. (2. *. tan (3.14159 /. float faces)))
  in
  (* I create another signal equal to pos
     because I need pos before it is created :( *)
  let pos2, set_pos2 = Eliom_shared.React.S.create (position, 0.) in
  let carousel, pos, size, swipe_pos =
    make
      ~a
      ~vertical
      ~position
      ?transition_duration
      ?inertia
      ?allow_overswipe
      ?update
      ?disabled
      ~make_transform:[%shared wheel_make_transform ~%z ~%faces ~%face_size]
      ~make_page_attribute:
        [%shared wheel_page_attribute ~%pos2 ~%z ~%faces ~%length]
      content
  in
  let _ = [%client
    (React.S.l2 (fun pos sp -> ~%set_pos2 (pos, sp)) ~%pos ~%swipe_pos
     : _ React.S.t) ]
  in
  carousel, pos, swipe_pos




(* To test, uncomment the following lines: *)

(* {client{ *)

(*    let _ = *)
(*      Lwt.async (fun () -> *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        let ev, send_ev = React.E.create () in *)
(*        let d, _ = make *)
(*            ~position:2 *)
(*            ~update:ev *)
(*            [div [h1 [pcdata "Coucou 1"]]; *)
(*             div [h1 [pcdata "Bonjour 2"]]; *)
(*             div [h1 [pcdata "Salut 3"]]; *)
(*             div [h1 [pcdata "Hello 4"]]; *)
(*            ] *)
(*        in *)
(*        Manip.appendToBody d; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Next; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Next; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Prev; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Next; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Prev; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 0); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 3); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 1); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 0); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 3); *)
(*        Lwt.return () *)
(*      ) *)

(*  }} *)
