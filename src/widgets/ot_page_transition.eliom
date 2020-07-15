[%%client.start]

open Js_of_ocaml
open Js_of_ocaml_lwt
open Eliom_content
open Html
open Html.D

type animation = Nil | Forward | Backward

module type PAGE_TRANSITION_CONF = sig
  type screenshot

  val screenshot_container
    :  screenshot
    -> Html_types.div_content Eliom_content.Html.elt
end

module URI : PAGE_TRANSITION_CONF with type screenshot = string = struct
  type screenshot = string

  let screenshot_container uri =
    let container = div ~a:[a_class ["ot-page-transition-ss-container"]] [] in
    Manip.SetCss.backgroundImage container (Printf.sprintf "url(%s)" uri);
    container
end

let set_transition_duration elt t =
  let s = Js.string (Printf.sprintf "%.2fs" t) in
  let container_style = Js.Unsafe.coerce (To_dom.of_element elt)##.style in
  container_style##.transitionDuration := s

let cl_body_pre_forward = "ot-page-transition-body-pre-forward"
let cl_wrapper_post_backward = "ot-page-transition-wrapper-post-backward"
let cl_wrapper = "ot-page-transition-wrapper"
let cl_screenshot_post_forward = "ot-page-transition-screenshot-post-forward"

module Make (Conf : PAGE_TRANSITION_CONF) = struct
  type screenshot = Conf.screenshot

  let wrap_screenshot ?(a = []) ~transition_duration ~screenshot =
    let container = Conf.screenshot_container screenshot in
    let wrapper = div ~a:(a_class [cl_wrapper] :: a) [container] in
    set_transition_duration wrapper transition_duration;
    set_transition_duration container transition_duration;
    wrapper, container

  let forward_animation_ transition_duration cur_screenshot =
    let h = React.S.value Ot_size.height in
    let body = Of_dom.of_body Dom_html.document##.body in
    let%lwt () = Lwt_js_events.request_animation_frame () in
    let style = Js.Unsafe.coerce Dom_html.document##.body##.style in
    let initial_height = style##.height in
    let initial_transition_duration = style##.transitionDuration in
    let screenshot_wrapper, screenshot_container =
      wrap_screenshot
        ~a:[a_class ["ot-page-transition-wrapper-forward"]]
        ~transition_duration ~screenshot:cur_screenshot
    in
    Eliom_client.lock_request_handling ();
    Manip.appendToBody screenshot_wrapper;
    Manip.SetCss.heightPx body h;
    Manip.Class.add body cl_body_pre_forward;
    let%lwt () = Lwt_js_events.request_animation_frame () in
    set_transition_duration body transition_duration;
    let%lwt () = Lwt_js_events.request_animation_frame () in
    Manip.Class.add screenshot_container cl_screenshot_post_forward;
    Manip.Class.remove body cl_body_pre_forward;
    let%lwt () = Lwt_js.sleep transition_duration in
    Manip.removeSelf screenshot_wrapper;
    Manip.SetCss.height body (Js.to_string initial_height);
    style##.transitionDuration := initial_transition_duration;
    Eliom_client.unlock_request_handling ();
    Lwt.return_unit

  let forward_animation ?(transition_duration = 0.5) take_screenshot =
    try
      let wait_for_page_change, trigger_page_change = Lwt.wait () in
      Eliom_client.Page_status.oninactive ~once:true (fun () ->
          Lwt.wakeup trigger_page_change ());
      let f cur_screenshot =
        Lwt.async @@ fun () ->
        let%lwt () = wait_for_page_change in
        forward_animation_ transition_duration cur_screenshot
      in
      take_screenshot f; Lwt.return_unit
    with _ -> Lwt.return_unit

  let backward_animation_ transition_duration history_screenshot =
    let h = React.S.value Ot_size.height in
    let body = Of_dom.of_body Dom_html.document##.body in
    let%lwt () = Lwt_js_events.request_animation_frame () in
    let style = Js.Unsafe.coerce Dom_html.document##.body##.style in
    let initial_height = style##.height in
    let screenshot_wrapper, _ =
      wrap_screenshot
        ~a:[a_class ["ot-page-transition-wrapper-backward"]]
        ~transition_duration ~screenshot:history_screenshot
    in
    Eliom_client.lock_request_handling ();
    Manip.appendToBody screenshot_wrapper;
    Manip.SetCss.heightPx body h;
    let%lwt () = Lwt_js_events.request_animation_frame () in
    Manip.Class.add screenshot_wrapper cl_wrapper_post_backward;
    let%lwt () = Lwt_js.sleep transition_duration in
    Manip.removeSelf screenshot_wrapper;
    Manip.SetCss.height body (Js.to_string initial_height);
    Eliom_client.unlock_request_handling ();
    Lwt.return_unit

  let backward_animation ?(transition_duration = 0.5) take_screenshot =
    try
      let wait_for_page_change, trigger_page_change = Lwt.wait () in
      Eliom_client.Page_status.oninactive ~once:true (fun () ->
          Lwt.wakeup trigger_page_change ());
      let f cur_screenshot =
        Lwt.async @@ fun () ->
        let%lwt () = wait_for_page_change in
        backward_animation_ transition_duration cur_screenshot
      in
      take_screenshot f; Lwt.return_unit
    with _ -> Lwt.return_unit

  let install_global_handler ?transition_duration ~take_screenshot
      ~animation_type
    =
    let rec hc_handler ev =
      Eliom_client.onchangepage hc_handler;
      match animation_type ev with
      | Nil -> Lwt.return_unit
      | Forward -> forward_animation ?transition_duration take_screenshot
      | Backward -> backward_animation ?transition_duration take_screenshot
    in
    Eliom_client.onchangepage hc_handler
end

let install_global_handler_withURI =
  let module Pt = Make (URI) in
  Pt.install_global_handler
