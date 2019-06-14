[%%shared.start]

type state = Pulling | Ready | Loading | Succeeded | Failed

[%%client
open Eliom_content.Html]

open Eliom_content.Html.D

let%shared default_header =
  let open Eliom_content.Html in
  function
  | Some Succeeded -> [F.div ~a:[F.a_class ["ot-pull-refresh-icon-success"]] []]
  | Some Failed -> [F.div ~a:[F.a_class ["ot-pull-refresh-icon-failure"]] []]
  | Some _ -> [F.div ~a:[F.a_class ["ot-icon-animation-spinning"]] []]
  | None -> []

[%%client
open Js_of_ocaml

module type CONF = sig
  val dragThreshold : float
  val moveCount : int
  val headContainerHeight : unit -> int
  val container : Html_types.div Eliom_content.Html.D.elt
  val set_state : ?step:React.step -> state option -> unit
  val timeout : float
  val afterPull : unit -> bool Lwt.t
end

module Make (Conf : CONF) = struct
  let dragThreshold = Conf.dragThreshold
  let moveCount = min (max 100 Conf.moveCount) 500
  let dragStart = ref (-1)
  let percentage = ref 0.
  let joinRefreshFlag = ref false
  let refreshFlag = ref false
  let container = Conf.container
  let js_container = To_dom.of_element container

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
    let translateY = -. !percentage *. float_of_int moveCount in
    joinRefreshFlag := true;
    if -. !percentage > dragThreshold
    then Conf.set_state @@ Some Ready
    else Conf.set_state @@ Some Pulling;
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
    Conf.set_state @@ Some Loading;
    Manip.Class.add container "ot-pull-refresh-transition-on";
    js_container##.style##.transform
    := Js.string
         ("translateY("
         ^ (string_of_int @@ Conf.headContainerHeight ())
         ^ "px)");
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
    let open Js_of_ocaml_lwt.Lwt_js_events in
    Lwt.async (fun () -> touchstarts js_container touchstart_handler);
    Lwt.async (fun () -> touchmoves js_container touchmove_handler);
    Lwt.async (fun () -> touchends js_container touchend_handler);
    Lwt.async (fun () -> touchcancels js_container touchend_handler)
end]

let make ?(a = []) ?(dragThreshold = 0.3) ?(moveCount = 200)
    ?(refresh_timeout = 20.) ?(header = [%shared default_header]) ~content
    (afterPull : (unit -> bool Lwt.t) Eliom_client_value.t)
  =
  let state_s, set_state = Eliom_shared.React.S.create None in
  let headContainer =
    Eliom_content.Html.R.node
    @@ Eliom_shared.React.S.map
         [%shared
           let open Eliom_content.Html in
           fun s ->
             F.div ~a:[F.a_class ["ot-pull-refresh-head-container"]]
             @@ Eliom_shared.Value.local ~%header
             @@ s]
         state_s
  in
  let container =
    div ~a:[a_class ["ot-pull-refresh-container"]] [headContainer; content]
  in
  ignore
    [%client
      (let onload () =
         let module Ptr_conf = struct
           let set_state = ~%set_state
           let dragThreshold = ~%dragThreshold
           let moveCount = ~%moveCount
           let timeout = ~%refresh_timeout

           let headContainerHeight () =
             (To_dom.of_element ~%headContainer)##.scrollHeight

           let container = ~%container
           let afterPull = ~%afterPull
         end
         in
         let module Ptr = Make (Ptr_conf) in
         Ptr.init ()
       in
       Eliom_client.onload onload
        : unit)];
  let open Eliom_content.Html in
  F.div ~a:(F.a_class ["ot-pull-refresh-wrapper"] :: a) [container]
