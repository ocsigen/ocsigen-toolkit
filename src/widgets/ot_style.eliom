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

let style elt = Dom_html.window##getComputedStyle elt

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

let top    elt = parse_px (style elt)##.top
let bottom elt = parse_px (style elt)##.bottom
let left   elt = parse_px (style elt)##.left
let right  elt = parse_px (style elt)##.right

let marginTop    e = float_of_px (style e)##.marginTop
let marginBottom e = float_of_px (style e)##.marginBottom
let marginLeft   e = float_of_px (style e)##.marginLeft
let marginRight  e = float_of_px (style e)##.marginRight
