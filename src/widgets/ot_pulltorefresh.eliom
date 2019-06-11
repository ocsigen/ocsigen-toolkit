[%%shared.start]

[%%client
open Eliom_content.Html]

open Eliom_content.Html.D

[%%client
module type CONF = sig
  val dragThreshold : float
  val moveCount : int
  val headContainerHeight : int
  val pullDownIcon : Html_types.div Eliom_content.Html.D.elt
  val loadingIcon : Html_types.div Eliom_content.Html.D.elt
  val successIcon : Html_types.div Eliom_content.Html.D.elt
  val failureIcon : Html_types.div Eliom_content.Html.D.elt
  val pullText : Html_types.span Eliom_content.Html.D.elt
  val container : Html_types.div Eliom_content.Html.D.elt
  val pullDownText : string
  val releaseText : string
  val loadingText : string
  val successText : string
  val failureText : string
  val rotateGradually : bool
  val blockPullIcon : bool
  val afterPull : unit -> bool Lwt.t
end

module Make (Conf : CONF) = struct
  let dragThreshold = Conf.dragThreshold
  let moveCount = min (max 100 Conf.moveCount) 500
  let dragStart = ref (-1)
  let percentage = ref 0.
  let joinRefreshFlag = ref false
  let refreshFlag = ref false
  let pullText = To_dom.of_element Conf.pullText
  let container = Conf.container
  let js_container = To_dom.of_element container

  let show =
    let icon_list =
      [Conf.pullDownIcon; Conf.loadingIcon; Conf.successIcon; Conf.failureIcon]
    in
    fun elt ->
      icon_list
      |> List.iter (fun x ->
             if x == elt
             then Manip.Class.remove x "ot-pull-refresh-display-none"
             else Manip.Class.add x "ot-pull-refresh-display-none")

  let touchstart_handler ev _ =
    Dom_html.stopPropagation ev;
    (if !refreshFlag || !joinRefreshFlag
    then Dom.preventDefault ev
    else
      let touch = ev##.changedTouches##item 0 in
      Js.Optdef.iter touch (fun touch -> dragStart := touch##.clientY);
      Manip.Class.remove container "ot-pull-refresh-transition-on";
      show Conf.pullDownIcon;
      if Conf.rotateGradually
      then Manip.Class.remove Conf.pullDownIcon "ot-pull-refresh-transition-on"
      else Manip.Class.remove Conf.pullDownIcon "ot-pull-refresh-up");
    Lwt.return_unit

  let touchmove_handler_ ev =
    Dom.preventDefault ev;
    let translateY = -. !percentage *. float_of_int moveCount in
    joinRefreshFlag := true;
    (if Conf.rotateGradually
    then
      let rotate_deg = int_of_float (-180. *. !percentage /. dragThreshold) in
      let rotate_deg =
        if Conf.blockPullIcon
        then min 180 rotate_deg
        else min 360 (2 * rotate_deg)
      in
      (To_dom.of_element Conf.pullDownIcon)##.style##.transform
      := Js.string ("rotate(" ^ string_of_int rotate_deg ^ "deg)"));
    if -. !percentage > dragThreshold
    then (
      pullText##.textContent := Js.some (Js.string Conf.releaseText);
      if not Conf.rotateGradually
      then Manip.Class.add Conf.pullDownIcon "ot-pull-refresh-up")
    else (
      pullText##.textContent := Js.some (Js.string Conf.pullDownText);
      if not Conf.rotateGradually
      then Manip.Class.remove Conf.pullDownIcon "ot-pull-refresh-up");
    js_container##.style##.transform
    := Js.string ("translateY(" ^ string_of_float translateY ^ "px)")

  let touchmove_handler ev _ =
    Dom_html.stopPropagation ev;
    if !dragStart >= 0
    then
      if !refreshFlag
      then Dom.preventDefault ev
      else if ev##.touches##.length = 1
      then (
        let target = ev##.changedTouches##item 0 in
        Js.Optdef.iter target (fun target ->
            percentage :=
              float_of_int (!dragStart - target##.clientY)
              /. float_of_int Dom_html.window##.screen##.height);
        (*move the container if and only if scrollTop = 0 and
            the page is scrolled down*)
        if Dom_html.document##.body##.scrollTop = 0 && !percentage < 0.
        then touchmove_handler_ ev
        else joinRefreshFlag := false);
    Lwt.return_unit

  let refresh () =
    Manip.Class.add container "ot-pull-refresh-transition-on";
    pullText##.textContent := Js.some (Js.string Conf.loadingText);
    show Conf.loadingIcon;
    js_container##.style##.transform
    := Js.string
         ("translateY(" ^ string_of_int Conf.headContainerHeight ^ "px)");
    refreshFlag := true;
    Lwt.async (fun () ->
        let%lwt b = Conf.afterPull () in
        if b
        then
          (*if page refresh succeeds*)
          ignore
            (Dom_html.window##setTimeout
               (Js.wrap_callback (fun () ->
                    pullText##.textContent :=
                      Js.some (Js.string Conf.successText);
                    show Conf.successIcon;
                    js_container##.style##.transform
                    := Js.string "translateY(0)";
                    refreshFlag := false))
               700.)
          (*if the page refreshing finishes instantaneously,
              setTimeout is used to show the animation*)
        else (
          (*if page refresh fails*)
          pullText##.textContent := Js.some (Js.string Conf.failureText);
          show Conf.failureIcon;
          js_container##.style##.transform := Js.string "translateY(0)";
          ignore
            (Dom_html.window##setTimeout
               (Js.wrap_callback (fun () -> refreshFlag := false))
               500.));
        Lwt.return_unit)

  let scroll_back () =
    (*scroll back to top if |percentage| < dragThreshold*)
    if !joinRefreshFlag
    then (
      Manip.Class.add container "ot-pull-refresh-transition-on";
      Manip.Class.add Conf.pullDownIcon "ot-pull-refresh-transition-on";
      (To_dom.of_element Conf.pullDownIcon)##.style##.transform
      := Js.string "rotate(0deg)";
      js_container##.style##.transform := Js.string "translateY(0)";
      ignore
        (Dom_html.window##setTimeout
           (Js.wrap_callback (fun () -> refreshFlag := false))
           500.))

  let touchend_handler ev _ =
    if !percentage < 0. && !dragStart >= 0
    then
      if !refreshFlag
      then Dom.preventDefault ev
      else (
        if -. !percentage > dragThreshold && !joinRefreshFlag
        then refresh ()
        else scroll_back ();
        (*reinitialize paramaters*)
        joinRefreshFlag := false;
        dragStart := -1;
        percentage := 0.);
    Lwt.return_unit

  let init () =
    let open Lwt_js_events in
    Lwt.async (fun () -> touchstarts js_container touchstart_handler);
    Lwt.async (fun () -> touchmoves js_container touchmove_handler);
    Lwt.async (fun () -> touchends js_container touchend_handler);
    Lwt.async (fun () -> touchcancels js_container touchend_handler)
end]

let make ?(dragThreshold = 0.3) ?(moveCount = 200)
    ?(pullDownIcon = div ~a:[a_class ["ot-pull-refresh-arrow-icon"]] [])
    ?(loadingIcon = div ~a:[a_class ["ot-pull-refresh-spinner"]] [])
    ?(successIcon = div ~a:[a_class ["ot-pull-refresh-icon-success"]] [])
    ?(failureIcon = div ~a:[a_class ["ot-pull-refresh-icon-failure"]] [])
    ?(pullText = span [])
    ?(headContainer = div ~a:[a_class ["ot-pull-refresh-head-container"]] [])
    ?(successText = "The page is refreshed")
    ?(failureText = "An error has occured")
    ?(pullDownText = "Pull down to refresh...")
    ?(releaseText = "Release to refresh...") ?(loadingText = "Loading...")
    ?(rotateGradually = false) ?(blockPullIcon = true) ?(alreadyAdded = false)
    ~content (afterPull : (unit -> bool Lwt.t) Eliom_client_value.t)
  =
  let container =
    div ~a:[a_class ["ot-pull-refresh-container"]] [headContainer; content]
  in
  ignore
    [%client
      (Manip.Class.add ~%pullDownIcon "ot-pull-refresh-pull-down-icon";
       if not ~%rotateGradually
       then Manip.Class.add ~%pullDownIcon "ot-pull-refresh-transition-on";
       if not ~%alreadyAdded
       then (
         let icon_list =
           [~%pullDownIcon; ~%loadingIcon; ~%successIcon; ~%failureIcon]
         in
         List.iter
           (fun elt -> Manip.Class.add elt "ot-pull-refresh-display-none")
           icon_list;
         Manip.appendChildren ~%headContainer icon_list;
         Manip.appendChild ~%headContainer ~%pullText);
       let onload () =
         let module Ptr_conf = struct
           let dragThreshold = ~%dragThreshold
           let moveCount = ~%moveCount

           let headContainerHeight =
             (To_dom.of_element ~%headContainer)##.scrollHeight

           let pullDownIcon = ~%pullDownIcon
           let loadingIcon = ~%loadingIcon
           let successIcon = ~%successIcon
           let failureIcon = ~%failureIcon
           let pullText = ~%pullText
           let container = ~%container
           let pullDownText = ~%pullDownText
           let releaseText = ~%releaseText
           let loadingText = ~%loadingText
           let successText = ~%successText
           let failureText = ~%failureText
           let rotateGradually = ~%rotateGradually
           let blockPullIcon = ~%blockPullIcon
           let afterPull = ~%afterPull
         end
         in
         let module Ptr = Make (Ptr_conf) in
         Ptr.init ()
       in
       Eliom_client.onload onload
        : unit)];
  let open Eliom_content.Html.F in
  div ~a:[a_class ["ot-pull-refresh-wrapper"]] [container]
