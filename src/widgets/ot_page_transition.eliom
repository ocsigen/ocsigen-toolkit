[%%client.start]
open Eliom_content
open Html
open Html.D

type animation = Nil|Forward|Backward

module type PAGE_TRANSITION_CONF = sig
  type screenshot
  val screenshot_container :
    screenshot -> Html_types.div_content Eliom_content.Html.elt
end

module URI : (PAGE_TRANSITION_CONF with type screenshot = string) = struct
  type screenshot = string

  let screenshot_container uri =
    let container = div ~a:[a_class ["ot-page-transition-container"]] [] in
    Manip.SetCss.backgroundImage container (Printf.sprintf "url(%s)" uri);
    container
end

let set_transition_duration elt t =
  let s = Js.string (Printf.sprintf "%.2fs" t) in
  let container_style =
    Js.Unsafe.coerce ((To_dom.of_element elt)##.style) in
  container_style##.transitionDuration := s

let cl_will_change = "ot-page-transition-will-change"

module Make (Conf:PAGE_TRANSITION_CONF) = struct
  type screenshot = Conf.screenshot
  let screenshot_list = Hashtbl.create 10

  let mem_screenshot id =
    Hashtbl.mem screenshot_list id

  let get_screenshot id =
    Hashtbl.find screenshot_list id

  let pop_screenshot id =
    let screenshot = Hashtbl.find screenshot_list id in
    Hashtbl.remove screenshot_list id;
    screenshot

  let push_screenshot id screenshot =
    let w,h = Ot_size.get_screen_size () in
    if mem_screenshot id
    then Hashtbl.replace screenshot_list id (screenshot,w,h)
    else Hashtbl.add screenshot_list id (screenshot,w,h)

  let wrap_screenshot screenshot transition_duration =
    let container = Conf.screenshot_container screenshot in
    let wrapper = div ~a:[a_class ["ot-page-transition-wrapper"]] [container] in
    set_transition_duration container transition_duration;
    wrapper,container

  let forward_animation_ transition_duration id =
    Eliom_client.onload @@ fun () ->
      Lwt.async @@ fun () ->
        let h = React.S.value Ot_size.height in
        let new_body = Of_dom.of_body Dom_html.document##.body in
        Manip.Class.add new_body cl_will_change;
        let style = Js.Unsafe.coerce Dom_html.document##.body##.style in
        let initial_height = style##.height in
        let initial_transition_duration = style##.transitionDuration in
        let screenshot,_,_ = get_screenshot id in
        let screenshot_wrapper,screenshot_container =
          wrap_screenshot screenshot transition_duration in
        Manip.appendToBody screenshot_wrapper;
        Manip.SetCss.heightPx new_body h;
        Manip.Class.add new_body "ot-page-transition-transform-1";
        let%lwt () = Lwt_js_events.request_animation_frame () in
        set_transition_duration new_body transition_duration;
        (* Wait for the next repaint. Other wise the setting of transition
           duration will not apply to body. *)
        let%lwt () = Lwt_js_events.request_animation_frame () in
        Manip.Class.add screenshot_container "ot-page-transition-transform-2";
        Manip.Class.remove new_body "ot-page-transition-transform-1";
        let%lwt () = Lwt_js.sleep transition_duration in
        Manip.removeSelf screenshot_wrapper;
        Manip.SetCss.height new_body (Js.to_string initial_height);
        Manip.Class.remove new_body cl_will_change;
        style##.transitionDuration := initial_transition_duration;
        Lwt.return_unit

  let wait_for ~sleep ~cycles cond =
    let rec loop i =
      if not (cond ()) && i < cycles then
        let%lwt () = Lwt_js.sleep sleep in
        loop (i+1)
      else Lwt.return_unit
    in
    loop 0

  let forward_animation ?(transition_duration=0.5) take_screenshot id =
    try
      let screenshot = ref None in
      take_screenshot (fun ss -> screenshot := Some ss);
      forward_animation_ transition_duration id;
      wait_for ~sleep:0.01 ~cycles:100
        (fun () -> match !screenshot with
                     | None -> false
                     | Some ss -> push_screenshot id ss; true)
    with _ -> Lwt.return_unit

  let backward_animation_
      transition_duration history_screenshot current_screenshot =
    let current_screenshot_container =
      Conf.screenshot_container current_screenshot in
    let h = React.S.value Ot_size.height in
    Manip.SetCss.heightPx current_screenshot_container h;
    let history_screenshot_wrapper,history_screenshot_container =
      wrap_screenshot history_screenshot transition_duration in
    Manip.Class.add
      history_screenshot_container "ot-page-transition-transform-2";
    let temporary_body =
      D.body [current_screenshot_container; history_screenshot_wrapper] in
    Manip.Class.add temporary_body cl_will_change;
    Manip.replaceSelf
      (Of_dom.of_body Dom_html.document##.body) temporary_body;
    let%lwt () = Lwt_js_events.request_animation_frame () in
    set_transition_duration temporary_body transition_duration;
    let%lwt () = Lwt_js_events.request_animation_frame () in
    Manip.Class.add temporary_body "ot-page-transition-transform-1";
    Manip.Class.remove
      history_screenshot_container "ot-page-transition-transform-2";
    Manip.Class.remove temporary_body cl_will_change;
    Lwt.return_unit

  let backward_animation ?(transition_duration=0.5) take_screenshot id =
    try
      let scr_width,scr_height = Ot_size.get_screen_size () in
      let hist_screenshot,w,h = pop_screenshot id in
      if w = scr_width && h = scr_height
      then begin
        let f cur_screenshot = Lwt.async @@ fun () ->
          backward_animation_ transition_duration hist_screenshot cur_screenshot
        in
        take_screenshot f;
        Lwt_js.sleep transition_duration
      end
      else Lwt.return_unit
    with _ -> Lwt.return_unit

  let install_global_handler
      ?transition_duration ~take_screenshot ~animation_type =
    let backward = backward_animation ?transition_duration take_screenshot in
    let forward = forward_animation ?transition_duration take_screenshot in
    let rec hc_handler ev =
      Eliom_client.onchangepage hc_handler;
      match animation_type ev with
      | Nil -> Lwt.return_unit
      | Forward -> forward ev.Eliom_client.origin_id
      | Backward ->
        match ev.Eliom_client.target_id with
        | Some target_id -> backward target_id
        | None -> Lwt.return_unit
    in
    Eliom_client.onchangepage hc_handler
end

let install_global_handler_withURI =
  let module Pt = Make(URI) in Pt.install_global_handler
