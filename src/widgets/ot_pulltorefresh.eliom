[%%shared.start]

type state = Pulling | Ready | Loading | Succeeded | Failed

[%%client
open Eliom_content.Html]

open Eliom_content.Html.D

let%shared default_header =
  let open Eliom_content.Html in
  function
  | Some Loading -> [F.div ~a:[F.a_class ["ot-icon-animation-spinning"]] []]
  | _ -> []

[%%client
open Js_of_ocaml

module type CONF = sig
  val dragThreshold : float
  val scale : float
  val container : Html_types.div Eliom_content.Html.D.elt
  val set_state : ?step:React.step -> state option -> unit
  val timeout : float
  val afterPull : unit -> bool Lwt.t
end

module Make (Conf : CONF) = struct
  let dragThreshold = Conf.dragThreshold
  let dragStart = ref (-1)
  let distance = ref 0.
  let scale = Conf.scale
  let top = ref true
  let joinRefreshFlag = ref false
  let refreshFlag = ref false
  let container = Conf.container
  let js_container = To_dom.of_element container

  let scroll_handler () =
    let _, y = Dom_html.getDocumentScroll () in
    if y > 0 then top := false else top := true

  let touchstart_handler ev _ =
    Dom_html.stopPropagation ev;
    (if !refreshFlag || !joinRefreshFlag
    then Dom.preventDefault ev
    else
      let touch = ev##.changedTouches##item 0 in
      Js.Optdef.iter touch (fun touch -> dragStart := touch##.clientY);
      Manip.Class.remove container "ot-pull-refresh-transition-on";
      Conf.set_state @@ Some Pulling);
    Lwt.return_unit

  let touchmove_handler_ ev =
    Dom.preventDefault ev;
    let translateY = !distance in
    joinRefreshFlag := true;
    if !distance > dragThreshold
    then Conf.set_state @@ Some Ready
    else Conf.set_state @@ Some Pulling;
    js_container##.style##.transform
    := Js.string ("translateY(" ^ string_of_float translateY ^ "px)")

  let touchmove_handler ev _ =
    scroll_handler ();
    Dom_html.stopPropagation ev;
    if !dragStart >= 0
    then
      if !refreshFlag
      then Dom.preventDefault ev
      else if ev##.touches##.length = 1
      then (
        let target = ev##.changedTouches##item 0 in
        Js.Optdef.iter target (fun target ->
            distance :=
              Float.sqrt (float_of_int (- !dragStart + target##.clientY))
              *. scale);
        (*move the container if and only if at the top of the document and
            the page is scrolled down*)
        if !top && !distance > 0.
        then touchmove_handler_ ev
        else joinRefreshFlag := false);
    Lwt.return_unit

  let refresh () =
    Conf.set_state @@ Some Loading;
    Manip.Class.add container "ot-pull-refresh-transition-on";
    js_container##.style##.transform
    := Js.string
         ("translateY(" ^ (string_of_float @@ Conf.dragThreshold) ^ "px)");
    refreshFlag := true;
    Lwt.async (fun () ->
        let%lwt b =
          Lwt.pick
            [ Conf.afterPull ()
            ; (let%lwt () = Js_of_ocaml_lwt.Lwt_js.sleep Conf.timeout in
               Lwt.return_false) ]
        in
        if b
        then
          (*if page refresh succeeds*)
          ignore
            (Dom_html.window##setTimeout
               (Js.wrap_callback (fun () ->
                    Conf.set_state @@ Some Succeeded;
                    js_container##.style##.transform
                    := Js.string "translateY(0)";
                    refreshFlag := false))
               700.)
          (*if the page refreshing finishes instantaneously,
              setTimeout is used to show the animation*)
        else (
          (*if page refresh fails*)
          Conf.set_state @@ Some Failed;
          js_container##.style##.transform := Js.string "translateY(0)";
          ignore
            (Dom_html.window##setTimeout
               (Js.wrap_callback (fun () -> refreshFlag := false))
               500.));
        Lwt.return_unit)

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
           500.))

  let touchend_handler ev _ =
    if !top && !distance > 0. && !dragStart >= 0
    then
      if !refreshFlag
      then Dom.preventDefault ev
      else (
        if !distance > dragThreshold && !joinRefreshFlag
        then refresh ()
        else scroll_back ();
        (*reinitialize paramaters*)
        joinRefreshFlag := false;
        dragStart := -1;
        distance := 0.);
    Lwt.return_unit

  let init () =
    let open Js_of_ocaml_lwt.Lwt_js_events in
    Lwt.async (fun () -> touchstarts js_container touchstart_handler);
    Lwt.async (fun () -> touchmoves js_container touchmove_handler);
    Lwt.async (fun () -> touchends js_container touchend_handler);
    Lwt.async (fun () -> touchcancels js_container touchend_handler)
end]

let make ?(a = []) ?(app_only = true) ?(scale = 5.) ?(dragThreshold = 80.)
    ?(refresh_timeout = 20.) ?(header = [%shared default_header]) ~content
    (afterPull : (unit -> bool Lwt.t) Eliom_client_value.t)
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
