[%%shared.start]

[%%client
  open Eliom_content.Html
  open Regexp
]

open Eliom_content.Html.D

[%%client

  module type PULLTOREFRESH = sig
    val dragThreshold : float
    val moveCount : int
    val headContainerHeight : int
    val pullText :  Html_types.span  Eliom_content.Html.D.elt
    val icon :  Html_types.div  Eliom_content.Html.D.elt
    val container :  Html_types.div  Eliom_content.Html.D.elt
    val pullIconClass : string list
    val loadingIconClass : string list
    val successClass : string list
    val failureClass : string list
    val pullDownText : string
    val releaseText : string
    val loadingText : string
    val successText : string
    val failureText : string
    val rotateGradually : bool
    val blockPullIcon : bool
    val afterPull : unit -> bool Lwt.t
  end

  module Make(Elt:PULLTOREFRESH) = struct
    let dragThreshold = Elt.dragThreshold 
    let moveCount  = min (max 100 Elt.moveCount) 500
    let dragStart = ref (-1)
    let percentage = ref 0.
    let joinRefreshFlag = ref false
    let refreshFlag = ref false
    let pullText = To_dom.of_element Elt.pullText
    let icon = Elt.icon
    let container = Elt.container
    let js_container = To_dom.of_element container

    let touchstart_handler ev _ =
      Dom_html.stopPropagation ev;
      if !refreshFlag || !joinRefreshFlag then Dom.preventDefault ev
      else 
        begin
          let touch = ev##.changedTouches##item(0) in
          Js.Optdef.iter touch (fun touch -> dragStart:= touch##.clientY);
          Manip.Class.remove container "ot-transition-on";
          Manip.Class.removes icon Elt.successClass;
          Manip.Class.removes icon Elt.failureClass;
          Manip.Class.adds icon Elt.pullIconClass;
          if  Elt.rotateGradually then
            Manip.Class.remove icon "ot-transition-on"
          else
            Manip.Class.remove icon "ot-up" ;
        end; 
      Lwt.return_unit

    let touchmove_handler_ ev =
      Dom.preventDefault ev;
      let translateY = -. !percentage *. (float_of_int moveCount)  in
      joinRefreshFlag := true;
      if Elt.rotateGradually then
        begin
          let rotate_deg = int_of_float (-180. *. !percentage /. dragThreshold) in
          let rotate_deg = if Elt.blockPullIcon then min 180 rotate_deg else min 360 (2*rotate_deg) in
          (To_dom.of_element icon)##.style##.transform := 
            Js.string ("rotate("^ (string_of_int rotate_deg) ^"deg)")
        end;
      if  -. !percentage > dragThreshold then
        begin
          pullText##.textContent := Js.some (Js.string Elt.releaseText);
          if not Elt.rotateGradually then Manip.Class.add icon "ot-up"
        end
      else
        begin
          pullText##.textContent := Js.some (Js.string Elt.pullDownText);
          if not Elt.rotateGradually then Manip.Class.remove icon "ot-up"
        end;
      js_container##.style##.transform := Js.string ("translate3d(0," ^ (string_of_float translateY) ^ "px,0)")

    let touchmove_handler ev _ =
      (*do nothing if the page is being refreshed*)
      Dom_html.stopPropagation ev;
      if !dragStart >= 0 then
        begin
          if !refreshFlag then
            Dom.preventDefault ev
          else if ev##.touches##.length=1 then
            begin
              let target = ev##.changedTouches##item(0) in
              Js.Optdef.iter target (fun target ->
                percentage := (float_of_int (!dragStart - target##.clientY))/. (float_of_int Dom_html.window##.screen##.height) 
              );
              if Dom_html.document##.body##.scrollTop = 0 && !percentage<0. then touchmove_handler_ ev else joinRefreshFlag:=false
            end
        end;
      Lwt.return_unit

    let refresh () =
      Manip.Class.add container "ot-transition-on";
      pullText##.textContent := Js.some ( Js.string Elt.loadingText);
      Manip.Class.removes icon Elt.pullIconClass;
      Manip.Class.adds icon Elt.loadingIconClass;
      (*let translate_y = string_of_int (int_of_float (Ot_size.client_height (To_dom.of_element icon))) in*)
      (*let translate_y =string_of_int ((To_dom.of_element icon)##.scrollHeight) in*)
      js_container##.style##.transform := Js.string ("translate3d(0,"^(string_of_int Elt.headContainerHeight)^"px,0)");
      refreshFlag := true;
      Lwt.async ( 
        fun () ->
          let%lwt b = Elt.afterPull () in
          if b then
            ignore( Dom_html.window##setTimeout (Js.wrap_callback (
              fun () -> 
                pullText##.textContent := Js.some (Js.string Elt.successText);
                Manip.Class.removes icon Elt.loadingIconClass;
                Manip.Class.adds icon Elt.successClass;
                js_container##.style##.transform := Js.string ("translate3d(0,0,0)");
                refreshFlag:=false)) 700.)
          else
            begin
              pullText##.textContent := Js.some (Js.string Elt.failureText) ;
              Manip.Class.removes icon Elt.loadingIconClass;
              Manip.Class.adds icon Elt.failureClass;
              js_container##.style##.transform := Js.string ("translate3d(0,0,0)");
              ignore (
                Dom_html.window##setTimeout (Js.wrap_callback (
                  fun () -> 
                    refreshFlag := false;
                ))  500.;
              )
            end;
          Lwt.return_unit )

    let scroll_back () =
      if !joinRefreshFlag then
        begin
          Manip.Class.add container "ot-transition-on";
          Manip.Class.add icon "ot-transition-on";
          (To_dom.of_element icon)##.style##.transform := Js.string ("rotate(0deg)") ;
          js_container##.style##.transform := Js.string ("translate3d(0,0,0)");
          ignore (
            Dom_html.window##setTimeout (Js.wrap_callback (
              fun () -> 
                refreshFlag := false;
            ))  500.;
          )
        end

    let touchend_handler ev _ =
      if !percentage<0. && !dragStart >= 0 then
        if !refreshFlag then
          Dom.preventDefault ev 
        else 
          begin
            if -. !percentage > dragThreshold && !joinRefreshFlag then
              refresh ()
            else
              scroll_back ();
            (*reinitialize paramaters*)
            joinRefreshFlag := false;
            dragStart := -1;
            percentage := 0.
          end;
      Lwt.return_unit

    let init () =
      Lwt.async (fun () -> Lwt_js_events.touchstarts js_container touchstart_handler);
      Lwt.async (fun () -> Lwt_js_events.touchmoves js_container touchmove_handler);
      Lwt.async (fun () -> Lwt_js_events.touchends js_container touchend_handler);
      Lwt.async (fun () -> Lwt_js_events.touchcancels js_container touchend_handler);
  end
]

let make 
    ?(dragThreshold = 0.3) 
    ?(moveCount = 200)
    ?(successClass = ["ot-default-icon-success"])
    ?(failureClass = ["ot-default-icon-failure"])
    ?(pullIconClass = ["ot-default-arrow-icon"])
    ?(loadingIconClass = ["ot-default-spinner"])
    ?(headContainerClass = ["ot-default-head-container"])
    ?(successText = "The page is refreshed")
    ?(failureText = "An error has occured")
    ?(pullDownText = "Pull down to refresh...")
    ?(releaseText = "Release to refresh...")
    ?(loadingText = "Loading...")
    ?(rotateGradually = false)
    ?(blockPullIcon = true)
    ~content 
    (afterPull: (unit-> bool Lwt.t) Eliom_client_value.t) = 
  let icon = if rotateGradually then div [] else div ~a:[a_class ["ot-transition-on"]][] in
  let pullText = span [] in
  let headContainer = div ~a:[a_class ("ot-head-container"::headContainerClass)] [icon;pullText] in
  let container = div [ headContainer; content ] in
  ignore (
    [%client 
      (let onload = fun () ->
         let module Ptr_elt = 
         struct
           let dragThreshold = ~%dragThreshold
           let moveCount = ~%moveCount
           let headContainerHeight = (To_dom.of_element ~%headContainer)##.scrollHeight
           let pullText = ~%pullText
           let icon = ~%icon
           let container = ~%container
           let successClass = ~%successClass
           let failureClass = ~%failureClass
           let pullIconClass =  "ot-arrow-icon"::~%pullIconClass
           let loadingIconClass = ~%loadingIconClass
           let pullDownText = ~%pullDownText
           let releaseText = ~%releaseText
           let loadingText = ~%loadingText
           let successText = ~%successText
           let failureText = ~%failureText
           let rotateGradually = ~%rotateGradually
           let blockPullIcon = ~%blockPullIcon
           let afterPull = ~%afterPull
         end in 
         let module Ptr = Make(Ptr_elt) in
         Ptr.init();
       in
       Eliom_client.onload onload  : unit )
    ]);
  Eliom_content.Html.F.(div ~a:[a_class ["ot-pull-to-refresh-wrapper"]][container])
