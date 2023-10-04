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

[%%shared.start] (* shared by default, override as necessary *)

open Eliom_content.Html
open Eliom_shared.React.S.Infix

type t = T_Up | T_Down

let%client up_for_true = function true -> T_Up | _ -> T_Down

let display_toggle ?(up_txt = "up") ?(down_txt = "down") f =
  let open D in
  function
  | T_Up ->
      div
        ~a:[a_class ["ot-toggle"]]
        [ div ~a:[a_class ["ot-active"; "ot-up"]] [txt up_txt]
        ; div
            ~a:
              [ a_class ["ot-inactive"; "ot-down"]
              ; a_onclick [%client (fun _ -> ~%f T_Down : _ -> _)] ]
            [txt down_txt] ]
  | T_Down ->
      div
        ~a:[a_class ["ot-toggle"]]
        [ div
            ~a:
              [ a_class ["ot-inactive"; "ot-up"]
              ; a_onclick [%client (fun _ -> ~%f T_Up : _ -> _)] ]
            [txt up_txt]
        ; div ~a:[a_class ["ot-active"; "ot-down"]] [txt down_txt] ]

let make ?(init_up = false) ?up_txt ?down_txt
    ?(update : bool React.E.t Eliom_client_value.t option) ()
  =
  let e, f = Eliom_shared.React.S.create (if init_up then T_Up else T_Down) in
  let elt =
    D.div
      [ e
        >|= [%shared display_toggle ~%f ?up_txt:~%up_txt ?down_txt:~%down_txt]
        |> R.node ]
  in
  (match update with
  | Some update ->
      ignore
      @@ [%client
           (let f b = ~%f (up_for_true b) in
            Eliom_lib.Dom_reference.retain (To_dom.of_element ~%elt)
              ~keep:(Eliom_shared.React.E.map f ~%update)
            : unit)]
  | None -> ());
  elt, e >|= [%shared function T_Up -> true | _ -> false]
