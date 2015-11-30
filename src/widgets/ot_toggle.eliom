(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Vasilis Papavasileiou
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

[%%shared

open Eliom_content.Html5

open Eliom_shared.React.S.Infix

type t = T_Up | T_Down

]

[%%shared

let display_toggle
    ?up_txt:(up_txt = "up")
    ?down_txt:(down_txt = "down")
    f =
  let open D in
  function
  | T_Up ->
    div ~a:[a_class ["ot-toggle"]]
      [div ~a:[a_class ["ot-active"; "ot-up"]]
         [pcdata up_txt];
       div ~a:[a_class ["ot-inactive"; "ot-down"];
               a_onclick  [%client  fun _ -> ~%f T_Down ] ]
         [pcdata down_txt]]
  | T_Down ->
    div ~a:[a_class ["ot-toggle"]]
      [div ~a:[a_class ["ot-inactive"; "ot-up"];
               a_onclick  [%client  fun _ -> ~%f T_Up ] ]
         [pcdata up_txt];
       div ~a:[a_class ["ot-active"; "ot-down"]]
         [pcdata down_txt]]

]

[%%shared

let make ?init_up:(init_up = false) ?up_txt ?down_txt ?update () =
  let e, f =
    Eliom_shared.React.S.create (if init_up then T_Up else T_Down)
  in
  (match update with
   | Some update ->
     [%client (
        let f b = if b then ~%f T_Up else ~%f T_Down in
        React.E.map f ~%update |> ignore : unit)] |> ignore;
   | None ->
     ());
  e >|=
  [%shared
     display_toggle ~%f ?up_txt:~%up_txt ?down_txt:~%down_txt ] |>
  R.node,
  e >|= [%shared  function T_Up -> true | _ -> false ]

]
