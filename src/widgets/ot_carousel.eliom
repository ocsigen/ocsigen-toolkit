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

(* TODO:

 - Swiping: avoid small delay before starting transition (the movement seems
   to stop and restart)
 - Make transition duration depend on swiping speed

 - full_height: restore position of each page

*)


[%%shared
open Eliom_content.Html5
open Eliom_content.Html5.F
]

let%client clX ev =
  Js.Optdef.case ev##.changedTouches##(item (0))
    (fun () -> 0)
    (fun a -> a##.clientX)

let%client clY ev =
  Js.Optdef.case ev##.changedTouches##(item (0))
    (fun () -> 0)
    (fun a -> a##.clientY)

let%client add_transition elt =
  (Js.Unsafe.coerce (elt##.style))##.transition :=
    Js.string "-webkit-transform .7s, transform .7s"

let%client remove_transition elt =
  (Js.Unsafe.coerce (elt##.style))##.transition :=
    Js.string "-webkit-transform 0s, transform 0s"

let%shared make
    ?(a = [])
    ?(vertical = false)
    ?(position = 0)
    ?(update : [`Goto of int | `Next | `Prev ] React.event Eliom_client_value.t option)
    ?(disabled = Eliom_shared.React.S.const false)
    ?(full_height = `No)
    l =
  let a = (a :> Html5_types.div_attrib attrib list) in
  let pos_signal, pos_set = Eliom_shared.React.S.create position in
  let swipe_pos_sig = [%client (React.S.create 0. : _ * _) ] in
  let swipe_pos = [%client (fst ~%swipe_pos_sig : _ React.S.t) ] in
  let swipe_pos_set = [%client (snd ~%swipe_pos_sig : ?step:_ -> _ -> _) ] in
  (* We wrap all pages in a div in order to add class carpage,
     for efficiency reasons in CSS (avoids selector ".car2>*")*)
  let pages = List.map (fun e -> D.div ~a:[a_class ["carpage"]] [e]) l in
  let d2 = D.div ~a:[a_class ["car2"]] pages in
  let d = D.div ~a:(a_class ["carousel";
                             if vertical then "vertical" else "horizontal"]::a)
      [d2]
  in
  let maxi = List.length pages - 1 in
  let nb_visible_elements, set_nb_visible_elements =
    Eliom_shared.React.S.create 1
  in
  let _ = [%client (
    let vertical = ~%vertical in
    let d2 = To_dom.of_element ~%d2 in
    let d = To_dom.of_element ~%d in
    let width_element () =
      if vertical then d2##.offsetHeight else d2##.offsetWidth
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
    ~%set_nb_visible_elements (comp_nb_visible_elements ());
    let maxi () = ~%maxi - (React.S.value ~%nb_visible_elements) + 1 in
    let pos_signal = ~%pos_signal in
    let pos_set = ~%pos_set in
    let action = ref (`Move (0, 0)) in
    let animation_frame_requested = ref false in
    (**********************
       setting class active on visible pages (only)
    *)
    let set_active () =
      (* Adding class "active" to all visible pages *)
        List.iteri (fun i page ->
          Manip.Class.remove page "active";
          let pos = React.S.value pos_signal in
          if i >= pos && i < pos + React.S.value ~%nb_visible_elements
          then (* Page is visible *) Manip.Class.add page "active";
        )
          ~%pages
    in
    (********************** end *)
    (**********************
       full_height
    *)
    let set_top_margin () =
      let dist = match ~%full_height with
        (* In full height mode, during swipe,
           we add margin on top of columns,
           to adapt it to the current scroll position of page.
        *)
        | `No -> None
        | `No_header -> Some 0
        | `Header f -> Some (f ())
      in
      Eliom_lib.Option.iter
        (fun dist ->
           let delta = max 0 (dist - Ot_size.client_top d2) in
           let pos = React.S.value pos_signal in
           List.iteri
             (fun i coli ->
                if i <> pos
                then
                  let coli' = To_dom.of_element coli in
                  coli'##.style##.marginTop :=
                    Js.string (string_of_int delta ^ "px")
             )
             ~%pages)
        dist
    in
    let unset_top_margin () =
      (* In full height mode, we remove the margins and set the correct
         scroll position for the page *)
      let dist = match ~%full_height with
        (* In full height mode, during swipe,
           we add margin on top of columns,
           to adapt it to the current scroll position of page.
        *)
        | `No -> None
        | `No_header -> Some 0
        | `Header f -> Some (f ())
      in
      Eliom_lib.Option.iter
        (fun dist ->
           let pos = React.S.value pos_signal in
           let delta = - (max 0 (dist - Ot_size.client_top d2)) in
           List.iteri
             (fun i coli ->
                let coli' = To_dom.of_element coli in
                coli'##.style##.marginTop := Js.string "0px"
             )
             ~%pages;
           (Js.Unsafe.coerce Dom_html.window)##scrollBy 0 delta)
        dist
    in
    (********************** end *)
    let set_position ?transitionend pos =
      let s =
        Js.string @@
        if vertical
        then Printf.sprintf "translate3d(0, %.3f%%, 0)" (-. float pos *. 100.)
        else Printf.sprintf "translate3d(%.3f%%, 0, 0)" (-. float pos *. 100.)
        (* then Printf.sprintf "translate(0, %.3f%%)" (-. float pos *. 100.) *)
        (* else Printf.sprintf "translate(%.3f%%, 0)" (-. float pos *. 100.) *)
        (* translate3d possibly more efficient on some devices ... *)
        (* If you switch back to translate,
           please explain why in comments. *)
      in
      (Js.Unsafe.coerce (d2##.style))##.transform := s;
      (Js.Unsafe.coerce (d2##.style))##.webkitTransform := s;
      pos_set pos;
      set_active ();
      Eliom_lib.Option.iter
        (fun f ->
           Lwt.async (fun () ->
             ~%swipe_pos_set 0.;
             let%lwt () = Lwt_js_events.transitionend d2 in
             unset_top_margin ();
             Lwt.return ()))
        transitionend
    in
    set_position ~%position;
    (*VVV I recompute the size everytime we touch the carousel
        and when the window is resized (?).
        Should be: every time the carousel size or content size changes
        and/or: provide a function to recompute size *)
    let update_size =
      React.S.map
        (fun _ -> ~%set_nb_visible_elements (comp_nb_visible_elements ()))
        (if vertical then Ot_size.height else Ot_size.width)
    in
    Eliom_client.onunload
      (fun () -> React.S.stop ~strong:true update_size; None);
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
               let sign = if delta < 0 then " - " else " + " in
               let pos = Eliom_shared.React.S.value pos_signal in
               ~%swipe_pos_set (-. (float delta) /. float width_element);
               let s = Printf.sprintf
                   "translate3d(%scalc(%d%%%s%dpx), 0%s)"
                   (* "translate(%scalc(%d%%%s%dpx)%s)" *)
                   (if vertical then "0, " else "")
                   (-pos * 100)
                   sign
                   (abs delta)
                   (if vertical then "" else ", 0")
               in
               (Js.Unsafe.coerce (d2##.style))##.transform := s;
               (Js.Unsafe.coerce (d2##.style))##.webkitTransform := s;
             | `Goback position
             | `Change position ->
               set_top_margin ();
               action := `Move (0, 0);
               set_position ~transitionend:unset_top_margin position);
            Lwt.return ()
          end
          else Lwt.return ()
      else Lwt.return ()
    in
    let status = ref (`Start (0, 0)) in
    let onpan ev _ =
      (match !status with
       | `Start (startx, starty) ->
         set_top_margin ();
         status :=
           if abs (if vertical
                   then clX ev - startx
                   else clY ev - starty) >= 10 then
             `Aborted
           else
             if abs (if not vertical
                     then clX ev - startx
                     else clY ev - starty) >= 10
             then `Ongoing (startx, starty, width_element ())
             else !status
       | _ -> ());
      (match !status with
       | `Ongoing (startx, starty, width_element) ->
         Dom.preventDefault ev;
         let delta =
           if vertical
           then clY ev - starty
           else clX ev - startx
         in
         Lwt.async
           (fun () -> perform_animation (`Move (delta, width_element)))
       | _ -> ());
      Lwt.return ()
    in
    (* let hammer = Hammer.make_hammer d2 in *)
    Lwt.async (fun () -> Lwt_js_events.touchstarts d (fun ev aa ->
      status := `Start (clX ev, clY ev);
      remove_transition d2;
      onpan ev aa));
    Lwt.async (fun () -> Lwt_js_events.touchmoves d onpan);
    Lwt.async (fun () -> Lwt_js_events.touchends d (fun ev _ ->
      add_transition d2;
      match !status with
      | `Start (startx, starty)
      | `Ongoing (startx, starty, _) ->
        let width, delta =
          if vertical
          then (To_dom.of_element ~%d2)##.offsetHeight,
               (clY ev - starty)
          else (To_dom.of_element ~%d2)##.offsetWidth,
               (clX ev - startx)
        in
        let pos = Eliom_shared.React.S.value pos_signal in
        let newpos =
          if float (abs delta) > 0.3 *. float width
          then
            if delta > 0
            then (if pos > 0 then pos - 1 else pos)
            else (if pos < maxi () then pos + 1 else pos)
          else pos
        in
        if newpos <> pos
        then perform_animation (`Change newpos)
        else perform_animation (`Goback newpos)
      | _ -> Lwt.return ()));
    (* Hammer.bind_callback hammer *)
    (*   (if vertical then "swipedown" else "swiperight") *)
    (*   (fun ev -> *)
    (*      let pos = Eliom_shared.React.S.value pos_signal in *)
    (*      Lwt.async (fun () -> if pos > 0 *)
    (*                  then perform_animation (`Change (pos - 1)) *)
    (*                  else perform_animation (`Goback pos)) *)
    (*   ); *)
    (* Hammer.bind_callback hammer *)
    (*   (if vertical then "swipeup" else "swipeleft") *)
    (*   (fun ev -> *)
    (*      let pos = Eliom_shared.React.S.value pos_signal in *)
    (*      Lwt.async (fun () -> if pos < maxi () *)
    (*                  then perform_animation (`Change (pos + 1)) *)
    (*                  else perform_animation (`Goback pos)) *)
    (*   ); *)
    (* (\* Warning! set_pan_all_directions: *)
    (*    If we do that for horizontal carousel, *)
    (*    it will break vertical scrolling on safari/ipad and firefox mobile ... *)
    (*    We should probably allow only vertical pan for vertical carousel (?) *\) *)
    (* if vertical then Hammer.set_pan_all_directions hammer; *)
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
       then ["active"] else [] ] pos size

let%shared bullets
    ?(a = []) ?attributes
    ~(change : ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t)
    ~pos ~length
    ?(size = Eliom_shared.React.S.const 1) () =
  let a = (a :> Html5_types.ul_attrib attrib list) in
  let bullet i =
    let class_ = bullet_class i pos size in
    let a = match attributes with
      | None -> []
      | Some l -> (List.nth l i :> Html5_types.li_attrib attrib list)
    in
    li ~a:(a_class [ "bullet-nav-item" ]
           :: R.a_class class_
           :: a_onclick [%client fun _ -> ~%change (`Goto ~%i)]
           :: a)
      []
  in
  let rec aux acc i = if i = 0 then acc else aux (bullet (i-1)::acc) (i-1) in
  ul ~a:(a_class ["bullet-nav"]::a) (aux [] length)

let%shared ribbon
    ?(a = [])
    ~(change : ([> `Goto of int | `Next | `Prev ] -> unit) Eliom_client_value.t)
    ~pos
    ?(size = Eliom_shared.React.S.const 1)
    ?(initial_gap = 0)
    ?cursor
    l =
  let a = (a :> Html5_types.div_attrib attrib list) in
  let item i c =
    let class_ = bullet_class i pos size in
    D.li ~a:[ a_class ["car-ribbon-list-item"]
            ; R.a_class class_
            ; a_onclick  [%client fun _ -> ~%change (`Goto ~%i)]
            ]
      c
  in
  let l = List.mapi item l in
  let nb_pages = List.length l in
  let the_ul = D.ul ~a:[a_class ["car-ribbon-list"]] l in
  let cursor_elt =
    Eliom_lib.Option.map (fun c -> D.div ~a:[a_class ["car-cursor"]] []) cursor
  in
  let cursor_l = match cursor_elt with None -> [] | Some c -> [ c ] in
  let container = D.div ~a:(a_class ["car-ribbon"]::a) (the_ul::cursor_l) in
  ignore [%client (
    let the_ul = ~%the_ul in
    let nb_pages = ~%nb_pages in
    let container = ~%container in
    let container' = To_dom.of_element container in
    let initial_gap = ~%initial_gap in
    let the_ul' = To_dom.of_element the_ul in
    let containerwidth, set_containerwidth =
      React.S.create container'##.offsetWidth in
    let curleft, set_curleft = React.S.create initial_gap in
    Lwt.async (fun () ->
      let%lwt () = Ot_nodeready.nodeready container' in
      (* Ribbon position: *)
      set_containerwidth container'##.offsetWidth ;
      Ot_noderesize.noderesize (Ot_noderesize.attach container') (fun () ->
        set_containerwidth container'##.offsetWidth
      ) ;
      (* noderesize is not very reliable, for example if we remove the
         node from page and put it back.
         As a temporary workaround, a also update containerwidth
         when the browser window's size changes: *)
      let watch_width =
        React.S.map
          (fun _ -> set_containerwidth container'##.offsetWidth)
          Ot_size.width in
      Eliom_client.onunload
        (fun () -> React.S.stop ~strong:true watch_width; None);
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
         ignore
           (React.S.l5
              (fun pos offset size curleft _ ->
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
    (***)
    let start = ref None in
    let old_transition = ref (Js.string "") in
    let fun_touchstart clX ev =
      start := Some (clX ev);
      old_transition := (Js.Unsafe.coerce (the_ul'##.style))##.transition;
      (Js.Unsafe.coerce (the_ul'##.style))##.transition := Js.string "left 0s";
      Lwt_js_events.request_animation_frame ()
    in
    let fun_touchup clX ev =
      (Js.Unsafe.coerce (the_ul'##.style))##.transition := !old_transition;
      let%lwt () = Lwt_js_events.request_animation_frame () in
      (match !start with
       | Some s ->
         let cw = React.S.value containerwidth in
         let cl = (max (- the_ul'##.scrollWidth + cw/2)
                     (min (cw / 2) (React.S.value curleft + clX ev - s)))
         in
         let ul_width = (To_dom.of_element the_ul)##.scrollWidth in
         (* Limit the movement
            (make this configurable by optional parameter?): *)
         let cl = min initial_gap cl in
         set_curleft
           (max (React.S.value containerwidth - ul_width - initial_gap) cl);
         the_ul'##.style##.left :=
           Js.string (string_of_int (React.S.value curleft)^"px");
         start := None
       | _ -> ());
      Lwt.return ()
    in
    let fun_touchmoves clX ev _ =
      Dom.preventDefault ev;
      (match !start with
       | Some start ->
         let ul_width = (To_dom.of_element the_ul)##.scrollWidth in
         let pos = React.S.value curleft + clX ev - start in
         (* Limit the movement
            (make this configurable by optional parameter?): *)
         let pos = min initial_gap pos in
         let pos =
           max (React.S.value containerwidth - ul_width - initial_gap) pos
         in
         the_ul'##.style##.left := Js.string (string_of_int pos^"px");
       | _ -> ());
      Lwt.return ()
    in
    let clX' ev = ev##.clientX in
    Lwt.async (fun () ->
      Lwt_js_events.mousedowns container' (fun ev _ ->
        let%lwt () = fun_touchstart clX' ev in
        Lwt.pick
          [(let%lwt ev = Lwt_js_events.mouseup Dom_html.document in
            fun_touchup clX' ev);
           Lwt_js_events.mousemoves Dom_html.document (fun_touchmoves clX')]));
    Lwt.async (fun () ->
      Lwt_js_events.touchstarts container' (fun ev _ ->
        let%lwt () = fun_touchstart clX ev in
        Lwt.pick
          [(let%lwt ev = Lwt_js_events.touchend Dom_html.document in
            fun_touchup clX ev);
           Lwt_js_events.touchmoves Dom_html.document (fun_touchmoves clX)]));
    Lwt.return () : _)];
  container

let%shared blur = function true -> ["blurred"] | false -> []

let%shared previous ?(a = [])
    ~(change : ([> `Prev ] -> unit) Eliom_client_value.t)
    ~pos content =
  Form.button_no_value
    ~button_type:`Button
    ~a:(R.a_class (Eliom_shared.React.S.map [%shared fun p -> blur (p = 0)] pos)
        :: a_class ["car-prev"]
        :: a_onclick  [%client  fun _ -> ~%change `Prev ]
        :: (a :> Html5_types.button_attrib
                Eliom_content.Html5.attrib list) )
    content

let%shared next ?(a = [])
    ~(change : ([> `Next ] -> unit) Eliom_client_value.t) ~pos ~size ~length content =
  Form.button_no_value
    ~button_type:`Button
    ~a:(R.a_class (Eliom_shared.React.S.l2
                     [%shared fun p s -> blur (p + s >= ~%length)]
                     pos size)
        :: a_class ["car-next"]
        :: a_onclick  [%client  fun _ -> ~%change `Next ]
        :: (a :> Html5_types.button_attrib
                Eliom_content.Html5.attrib list) )
    content

(* (\* Menu + prev/next buttons *\) *)
(* let nav ?(a = []) ~change ~pos ?size l = *)
(*   let menu = menu ~change ~pos ?size l in *)
(*   let prev = button *)
(*       ~button_type:`Button *)
(*       ~a:[a_class ["car-prev"]; *)
(*           a_onclick {{ fun _ -> %change `Prev }}] *)
(*       [pcdata ([%i18n prev_col ())] *)
(*   in *)
(*   let next = button *)
(*       ~button_type:`Button *)
(*       ~a:[a_class ["car-next"]; *)
(*           a_onclick {{ fun _ -> %change `Next }}] *)
(*       [pcdata ([%i18n next_col ())] *)
(*   in *)
(*   D.div ~a:(a_class ["car-nav"]::a) [prev; menu; next] *)



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
