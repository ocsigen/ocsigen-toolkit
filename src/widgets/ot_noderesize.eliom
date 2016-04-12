(*
 * This is a redo of [ResizeSensor.js] which is MIT licensed, with few patches
 * https://github.com/marcj/css-element-queries/blob/master/src/ResizeSensor.js
 *)

(* KNOW ISSUES:
   - This only work with elements in the DOM (maybe that the element
     has to be displayd, need to check this).
     In case of a content loaded dynamically with js, watch a parent if possible
     or use a [onnodeready] event to attach [noderesize] listener.
   - If the element to be watched is not positionned, a [position: relative]
     will be applied
 *)

(* How to
   Lwt.async (fun () ->
     let div' = (To_dom.of_element div) in
     let%lwt () = Nodeready.nodeready container' in
     Noderesize.noderesize (Noderesize.init div) (fun () ->
        Firebug.console##log (Js.string "Resized") ;
      ) ) ;
*)

[%%client open Eliom_content.Html5 ]
[%%client open Eliom_content.Html5.F ]

[%%client type resize_sensor =
            { watched : Dom_html.element Js.t
            ; grow : Dom_html.element Js.t
            ; mutable grow_listener_id : Dom.event_listener_id option
            ; grow_child : Dom_html.element Js.t
            ; shrink : Dom_html.element Js.t
            ; mutable shrink_listener_id : Dom.event_listener_id option
            ; sensor : Dom_html.element Js.t } ]

let%client attach watched =
  let style = "display:block; position: absolute; \
               left: 0; top: 0; right: 0; bottom: 0; \
               overflow: hidden; z-index: -1000; visibility: hidden;" in
  let style_child = "position: absolute; left: 0; top: 0;" in
  let grow_child = D.div ~a:[ a_style style_child ] [] in
  let grow =
    D.div ~a:[ a_class ["resize-sensor-grow"] ; a_style style ] [ grow_child ]
  in
  let shrink =
    D.div ~a:[ a_class ["resize-sensor-shrink"] ; a_style style ]
      [ div ~a:[ a_style (style_child ^ " width: 200%; height: 200%;") ] [] ] in
  let sensor = D.div ~a:[ a_class ["resize-sensor"] ; a_style style ]
      [ grow ; shrink ] in
  let grow_child = To_dom.of_element grow_child in
  let grow = To_dom.of_element grow in
  let shrink = To_dom.of_element shrink in
  let sensor = To_dom.of_element sensor in
  if (Dom_html.window##getComputedStyle watched)##.position = Js.string "static"
  then watched##.style##.position := Js.string "relative" ;
  Dom.appendChild watched sensor ;
  { watched = (watched :> Dom_html.element Js.t)
  ; grow ; grow_child ; shrink ; sensor
  ; grow_listener_id = None ; shrink_listener_id = None }

let%client detach
    { watched ; sensor ; shrink_listener_id ; grow_listener_id ; _ } =
  Dom.removeChild watched sensor ;
  (match grow_listener_id with Some x -> Dom.removeEventListener x | _ -> ()) ;
  (match shrink_listener_id with Some x -> Dom.removeEventListener x | _ -> ())

let%client reset { grow ; grow_child ; shrink ; _ } =
  shrink##.scrollLeft := shrink##.scrollWidth ;
  shrink##.scrollTop := shrink##.scrollHeight ;
  grow_child##.style##.width :=
    Js.string ((string_of_int (grow##.offsetWidth + 1)) ^ "px") ;
  grow_child##.style##.height :=
    Js.string ((string_of_int (grow##.offsetHeight + 1)) ^ "px") ;
  grow##.scrollLeft := grow##.scrollWidth ;
  grow##.scrollTop := grow##.scrollHeight

let%client noderesize sensor f =
  let bind element =
    let w = ref element##.offsetWidth in
    let h = ref element##.offsetHeight in
    Dom.addEventListener element (Dom.Event.make "scroll")
      (Dom.handler (fun _ ->
         let w' = element##.offsetWidth in
         let h' = element##.offsetHeight in
         if w' <> !w || h' <> !h then f () ;
         w := w' ;
         h := h' ;
         reset sensor ;
         Js.bool true ) ) (Js.bool false) in
  reset sensor ;
  sensor.grow_listener_id <- Some (bind sensor.grow) ;
  sensor.shrink_listener_id <- Some (bind sensor.shrink)
