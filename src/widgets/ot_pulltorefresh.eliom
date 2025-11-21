[%%shared.start]

type state = Pulling | Ready | Loading | Succeeded | Failed

open%client Eliom_content.Html
open Eliom_content.Html.D

let%shared default_header =
  let open Eliom_content.Html in
  function
  | Some Loading -> [F.div ~a:[F.a_class ["ot-icon-animation-spinning"]] []]
  | _ -> []

[%%client
open Js_of_ocaml
open Js_of_ocaml
open Js_of_ocaml_eio

module type CONF = sig
  val dragThreshold : float
  val scale : float
  val container : Html_types.div Eliom_content.Html.D.elt
  val set_state : ?step:React.step -> state option -> unit
  val timeout : float
  val afterPull : unit -> bool
end

module Make (Conf : CONF) = struct
  let dragThreshold = Conf.dragThreshold
  let dragStart = ref (-1.)
  let scrollXStart = ref (-1.)
  let distance = ref 0.
  let scale = Conf.scale
  let top = ref true
  let scrollingX = ref false
  let joinRefreshFlag = ref false
  let refreshFlag = ref false
  let first_move = ref false
  let container = Conf.container
  let js_container = To_dom.of_element container

  let scroll_handler () =
    let _, y = Dom_html.getDocumentScroll () in
    if y > 0. then top := false else top := true

  let touchstart_handler ev =
    Dom_html.stopPropagation ev;
    if !refreshFlag || !joinRefreshFlag
    then Dom.preventDefault ev
    else
      let touch = ev##.changedTouches##item 0 in
      Js.Optdef.iter touch (fun touch ->
        dragStart := Js.to_float touch##.clientY;
        scrollXStart := Js.to_float touch##.clientX);
      first_move := true;
      Manip.Class.remove container "ot-pull-refresh-transition-on"

  let touchmove_handler_ ev =
    Dom.preventDefault ev;
    let translateY = !distance in
    joinRefreshFlag := true;
    if !distance > dragThreshold
    then Conf.set_state @@ Some Ready
    else Conf.set_state @@ Some Pulling;
    js_container##.style##.transform
    := Js.string ("translateY(" ^ string_of_float translateY ^ "px)")

  let touchmove_handler ev =
    scroll_handler ();
    if not !scrollingX
    then (
      Dom_html.stopPropagation ev;
      if !dragStart >= 0.
      then
        if !refreshFlag
        then Dom.preventDefault ev
        else if ev##.touches##.length = 1
        then (
          let target = ev##.changedTouches##item 0 in
          Js.Optdef.iter target (fun target ->
            let dY = -. !dragStart +. Js.to_float target##.clientY in
            distance := Float.sqrt dY *. scale;
            if !first_move
            then
              scrollingX :=
                abs_float (!scrollXStart -. Js.to_float target##.clientX)
                > abs_float dY);
          (*move the container if and only if at the top of the document and
            the page is scrolled down*)
          if !top && !distance > 0. && not !scrollingX
          then touchmove_handler_ ev
          else joinRefreshFlag := false));
    first_move := false

  let refresh () =
    Conf.set_state @@ Some Loading;
    Manip.Class.add container "ot-pull-refresh-transition-on";
    js_container##.style##.transform
    := Js.string
         ("translateY(" ^ (string_of_float @@ Conf.dragThreshold) ^ "px)");
    refreshFlag := true;
    Eliom_lib.fork (fun () ->
      let b =
        Eio.Fiber.any
          [ Conf.afterPull
          ; (fun () ->
              Js_of_ocaml_eio.Eio_js.sleep Conf.timeout;
              false) ]
      in
      if b
      then
        (*if page refresh succeeds*)
        ignore
          (Dom_html.window##setTimeout
             (Js.wrap_callback (fun () ->
                Conf.set_state @@ Some Succeeded;
                js_container##.style##.transform := Js.string "translateY(0)";
                refreshFlag := false))
             (Js.float 700.))
        (*if the page refreshing finishes instantaneously,
              setTimeout is used to show the animation*)
      else (
        (*if page refresh fails*)
        Conf.set_state @@ Some Failed;
        js_container##.style##.transform := Js.string "translateY(0)";
        ignore
          (Dom_html.window##setTimeout
             (Js.wrap_callback (fun () -> refreshFlag := false))
             (Js.float 500.))))

  let scroll_back () =
    Conf.set_state None;
    (*scroll back to top if |percentage| < dragThreshold*)
    if !joinRefreshFlag
    then (
      Manip.Class.add container "ot-pull-refresh-transition-on";
      js_container##.style##.transform := Js.string "translateY(0)";
      ignore
        (Dom_html.window##setTimeout
           (Js.wrap_callback (fun () -> refreshFlag := false))
           (Js.float 500.)))

  let touchend_handler ev =
    if !top && !distance > 0. && !dragStart >= 0.
    then
      if !refreshFlag
      then Dom.preventDefault ev
      else (
        if !distance > dragThreshold && !joinRefreshFlag
        then refresh ()
        else scroll_back ();
        (*reinitialize paramaters*)
        joinRefreshFlag := false;
        dragStart := -1.;
        distance := 0.);
    scrollXStart := -1.;
    scrollingX := false

  let init () =
    let open Js_of_ocaml_eio.Eio_js_events in
    Eio_js.start (fun () -> touchstarts js_container touchstart_handler);
    Eio_js.start (fun () -> touchmoves js_container touchmove_handler);
    Eio_js.start (fun () -> touchends js_container touchend_handler);
    Eio_js.start (fun () -> touchcancels js_container touchend_handler)
end]

let make
      ?(a = [])
      ?(app_only = true)
      ?(scale = 5.)
      ?(dragThreshold = 80.)
      ?(refresh_timeout = 20.)
      ?(header = [%shared default_header])
      ~content
      (afterPull : (unit -> bool) Eliom_client_value.t)
  =
  if app_only && not (Eliom_client.is_client_app ())
  then div ~a [content]
  else
    let state_s, set_state = Eliom_shared.React.S.create None in
    let headContainer =
      Eliom_content.Html.R.node
      @@ Eliom_shared.React.S.map
           [%shared
             let open Eliom_content.Html in
             fun s ->
               D.div ~a:[D.a_class ["ot-pull-refresh-head-container"]]
               @@ Eliom_shared.Value.local ~%header
               @@ s]
           state_s
    in
    let container =
      div ~a:[a_class ["ot-pull-refresh-container"]] [headContainer; content]
    in
    ignore
      [%client
        (let module Ptr_conf = struct
           let set_state = ~%set_state
           let scale = ~%scale
           let dragThreshold = ~%dragThreshold
           let timeout = ~%refresh_timeout
           let container = ~%container
           let afterPull = ~%afterPull
         end
         in
         let module Ptr = Make (Ptr_conf) in
         Ptr.init ()
         : unit)];
    let open Eliom_content.Html in
    F.div ~a:(F.a_class ["ot-pull-refresh-wrapper"] :: a) [container]
