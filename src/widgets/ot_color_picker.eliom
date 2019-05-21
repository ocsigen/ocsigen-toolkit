(* Ocsigen-widgets
 * http://www.ocsigen.org/ocsigen-widgets
 *
 * Copyright (C) 2014 Université Paris Diderot
 *      Enguerrand Decorne
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

[%%shared.start]

open Eliom_shared.React.S.Infix

let hsv_to_rgb h s v =
  let h = float_of_int h in
  let c = v *. s in
  let h1 = h /. 60. in
  let x = c *. (1. -. (abs_float ((mod_float h1 2.) -. 1.))) in
  let m = v -. c in
  let r, g, b =
    match h1 with
    | _ when h1 < 1. -> c,  x,  0.
    | _ when h1 < 2. -> x,  c,  0.
    | _ when h1 < 3. -> 0., c,  x
    | _ when h1 < 4. -> 0., x,  c
    | _ when h1 < 5. -> x,  0., c
    | _ when h1 <= 6. -> c,  0., x
    | _ -> 0., 0., 0.  in
  255. *. (r +. m),
  255. *. (g +. m),
  255. *. (b +. m)

let rgb_to_css (r, g, b) =
  Printf.sprintf "rgb(%d, %d, %d)" (int_of_float r) (int_of_float g) (int_of_float b)

let display_hue_selector ~setter ((sel_hue, sel_sat, sel_ltn) as sel) =
  let open Eliom_content.Html in
  let dim = 111 in
  let irange = Array.init dim (fun x -> x) in
  let step = 360.0 /. float_of_int dim in
  let cells =
    Array.map
      (fun i ->
        let hue = (float_of_int i *. step) in
        let is_selected = (hue >= float_of_int sel_hue) && (hue -. step < float_of_int sel_hue) in
        let bgcolor = Printf.sprintf "hsl(%.3f, %.3f%%, %.3f%%)" hue 100.0 50.0 in
        D.(
          div
            ~a:[
              a_class (
                if is_selected
                then ["ot-color-picker-hue-picker-cell"; "ot-color-picker-selected-cell"]
                else ["ot-color-picker-hue-picker-cell"]
              );
              a_style ("background-color: " ^ bgcolor);
              a_onmousedown
                [%client fun _ ->
                  let hue = int_of_float ~%hue in
                  ~%setter (hue, ~%sel_sat, ~%sel_ltn)
                ]
            ]
            []
        )
      )
      irange
  in
  D.(div
    ~a:[
      a_class ["ot-color-picker-hue-picker"]
    ]
    (Array.to_list cells))

(**
  * TODO: use a CSS3 gradient in each cell to make the grid look smoother
  *)
let display_sl_grid ~setter ((sel_hue, sel_sat, sel_ltn) as sel) =
  let open Eliom_content.Html in
  let hue = float_of_int sel_hue in
  let dim = 51 in
  let step = 1.0 /. float_of_int dim in
  let irange = Array.init (dim * dim) (fun x -> x) in
  let cell_dim = Printf.sprintf "%.3f%%" ((1.0 /. float_of_int dim) *. 100.0) in
  let rows = Array.init dim (fun _ -> []) in
  let () =
    Array.iter
      (fun i ->
        let row = i / dim in
        let col = i mod dim in
        let saturation = float_of_int row /. float_of_int dim in
        let lightness = 1.0 -. (float_of_int col /. float_of_int dim) in
        let is_selected = (saturation = sel_sat) && (lightness = sel_ltn) in
        let style' =
          Printf.sprintf
            "display: inline-block; background-color: %s; height: 100%%"
            (rgb_to_css (hsv_to_rgb (int_of_float hue) saturation lightness))
        in
        let el =
          D.(
            div
              ~a:[
                a_class (
                  if is_selected
                  then ["ot-color-picker-sl-picker-cell"; "ot-color-picker-selected-cell"]
                  else ["ot-color-picker-sl-picker-cell"]);
                a_style style';
                a_onmousedown
                  [%client fun _ ->
                    ~%setter (~%sel_hue, ~%saturation, ~%lightness)
                  ]
              ]
              []
          )
        in
        rows.(row) <- el :: rows.(row)
      )
      irange
  in
  let rows =
    Array.map
      (fun r ->
        D.(
          div
            ~a:[a_class ["ot-color-picker-sl-picker-row"]]
            r))
      rows
  in
  D.(
    div
      ~a:[a_class ["ot-color-picker-sl-picker"]]
      (Array.to_list rows)
  )

let display_aux ~setter ((sel_hue, sel_sat, sel_ltn) as sel) =
  let open Eliom_content.Html in
  D.(
    div ~a:[a_class ["ot-color-picker"]]
      [ display_hue_selector ~setter sel
      ; display_sl_grid ~setter sel
      ]
  )

let display cp_sig =
  let setter = snd cp_sig in
  fst cp_sig >|= [%shared display_aux ~setter:~%setter] |> Eliom_content.Html.R.node

let make () =
  let cp_sig = Eliom_shared.React.S.create (255, 1.0, 0.0) in
  ( display
      cp_sig
  , fst cp_sig
  )
