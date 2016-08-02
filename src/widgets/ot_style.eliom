(*TODO: interface file*)
(*TODO: reactive programming*)

(* This module is all about easier access to getComputedStyle *)
(* See: https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration *)
(* See: https://ocsigen.org/js_of_ocaml/api/type_Dom_html.cssStyleDeclaration *)

(*
val marginTop    : Dom_html.element Js.t -> float
val marginBottom : Dom_html.element Js.t -> float
val marginLeft   : Dom_html.element Js.t -> float
val marginRight  : Dom_html.element Js.t -> float

(** [parse_px "118.64px" = Some 118.64] *)
val parse_px : Js.js_string Js.t -> float option
*)

[%%client.start]

open Eliom_content.Html

let parse_px str =
  let str = Js.to_string str in
  let len = String.length str in
  try
    let num = String.sub str 0 (len - 2) in
    match String.sub str (len - 2) 2 with
    | "px" -> Some (float_of_string num)
    | _ -> None
  with Invalid_argument _ | Match_failure _ -> None

let float_of_px str = match parse_px str with | None -> 0.0 | Some x -> x
let px_of_float px = Printf.sprintf "%gpx" px

let style elt = Dom_html.window##getComputedStyle elt

(*TODO: constructors instead of strings?*)
let display elt = Js.to_string (style elt)##.display
let visibility elt = Js.to_string (style elt)##.visibility

(*TODO: not well-tested! does this work on all browsers? *)
let invisible elt =
  (*https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement/offsetParent*)
  elt##.offsetParent = Js.null ||
  display elt = "none" ||
  visibility elt = "hidden"

let top    elt = parse_px (style elt)##.top
let bottom elt = parse_px (style elt)##.bottom
let left   elt = parse_px (style elt)##.left
let right  elt = parse_px (style elt)##.right

let marginTop    e = float_of_px (style e)##.marginTop
let marginBottom e = float_of_px (style e)##.marginBottom
let marginLeft   e = float_of_px (style e)##.marginLeft
let marginRight  e = float_of_px (style e)##.marginRight

let set_top    e v = Manip.SetCss.top    e @@ px_of_float v
let set_bottom e v = Manip.SetCss.bottom e @@ px_of_float v
let set_left   e v = Manip.SetCss.left   e @@ px_of_float v
let set_right  e v = Manip.SetCss.right  e @@ px_of_float v
let set_width  e v = Manip.SetCss.width  e @@ px_of_float v
let set_height e v = Manip.SetCss.height e @@ px_of_float v
