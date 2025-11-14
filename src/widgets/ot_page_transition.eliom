[%%client.start]

open Js_of_ocaml
open Js_of_ocaml_lwt
open Eliom_content
open Html
open Html.D
open Lwt.Syntax

type animation = Nil | Forward | Backward

module type PAGE_TRANSITION_CONF = sig
  type screenshot

  val screenshot_container :
     screenshot option
    -> Html_types.div_content Eliom_content.Html.elt
end

module URI : PAGE_TRANSITION_CONF with type screenshot = string = struct
  type screenshot = string

  let screenshot_container uri =
    let container = div ~a:[a_class ["ot-page-transition-ss-container"]] [] in
    let str =
      match uri with None -> "" | Some u -> Printf.sprintf "url(%s)" u
    in
    Manip.SetCss.backgroundImage container str;
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

module Option = struct
  let may f = function Some x -> f x | None -> ()
end

module Make (Conf : PAGE_TRANSITION_CONF) = struct
  type screenshot = Conf.screenshot

  let wrap_screenshot ?(a = []) ~transition_duration ~screenshot =
    let container = Conf.screenshot_container screenshot in
    let wrapper = div ~a:(a_class [cl_wrapper] :: a) [container] in
    set_transition_duration wrapper transition_duration;
    set_transition_duration container transition_duration;
    wrapper, container

  let forward_animation_ transition_duration screenshot =
    let body = Of_dom.of_body Dom_html.document##.body in
    let style = Js.Unsafe.coerce Dom_html.document##.body##.style in
    let initial_transition_duration = style##.transitionDuration in
    let screenshot_wrapper, screenshot_container =
      match screenshot with
      | Some screenshot ->
          let screenshot_wrapper, screenshot_container =
            wrap_screenshot
              ~a:[a_class ["ot-page-transition-wrapper-forward"]]
              ~transition_duration ~screenshot:(Some screenshot)
          in
          Some screenshot_wrapper, Some screenshot_container
      | None -> None, None
    in
    Eliom_client.lock_request_handling ();
    Option.may Manip.appendToBody screenshot_wrapper;
    Manip.Class.add body cl_body_pre_forward;
    let* () = Lwt_js_events.request_animation_frame () in
    let* () = Lwt_js_events.request_animation_frame () in
    set_transition_duration body transition_duration;
    Option.may
      (fun sc -> Manip.Class.add sc cl_screenshot_post_forward)
      screenshot_container;
    Manip.Class.remove body cl_body_pre_forward;
    let* () = Lwt_js.sleep transition_duration in
    Option.may Manip.removeSelf screenshot_wrapper;
    style##.transitionDuration := initial_transition_duration;
    Eliom_client.unlock_request_handling ();
    Lwt.return_unit

  let forward_animation ?(transition_duration = 0.5) take_screenshot =
    let wait_for_page_change, trigger_page_change = Lwt.wait () in
    Eliom_client.Page_status.oninactive ~once:true (fun () ->
      Lwt.wakeup trigger_page_change ());
    let fa ss =
      Lwt.async @@ fun () ->
      let* () = wait_for_page_change in
      forward_animation_ transition_duration ss
    in
    let f screenshot = fa @@ Some screenshot in
    (try take_screenshot f with _ -> fa None);
    Lwt.return_unit

  let backward_animation_ transition_duration screenshot =
    let screenshot_wrapper, _ =
      wrap_screenshot
        ~a:[a_class ["ot-page-transition-wrapper-backward"]]
        ~transition_duration ~screenshot
    in
    Eliom_client.lock_request_handling ();
    Manip.appendToBody screenshot_wrapper;
    let* () = Lwt_js_events.request_animation_frame () in
    let* () = Lwt_js_events.request_animation_frame () in
    Manip.Class.add screenshot_wrapper cl_wrapper_post_backward;
    let* () = Lwt_js.sleep transition_duration in
    Manip.removeSelf screenshot_wrapper;
    Eliom_client.unlock_request_handling ();
    Lwt.return_unit

  let backward_animation ?(transition_duration = 0.5) take_screenshot =
    let wait_for_page_change, trigger_page_change = Lwt.wait () in
    Eliom_client.Page_status.oninactive ~once:true (fun () ->
      Lwt.wakeup trigger_page_change ());
    let ba ss =
      Lwt.async @@ fun () ->
      let* () = wait_for_page_change in
      backward_animation_ transition_duration ss
    in
    let f screenshot = ba @@ Some screenshot in
    (try take_screenshot f with _ -> ba None);
    Lwt.return_unit

  let install_global_handler
        ?transition_duration
        ~take_screenshot
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
