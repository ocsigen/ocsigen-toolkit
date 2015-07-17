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

{shared{

open Eliom_content.Html5.D

type t = T_Up | T_Down

 }}

{client{

module Eliom_lib = struct
  (* copied from Eliom_csreact ; find a proper place for this *)
  include Eliom_lib
  let create_shared_value _ x = x
end

let r_node a = Eliom_content.Html5.R.node a

}}

{server{ let r_node a = Eliom_csreact.R.node a }} ;;

{shared{

let display_toggle
    ?up_txt:(up_txt = "up")
    ?down_txt:(down_txt = "down")
    f =
  function
  | T_Up ->
    div ~a:[a_class ["ot-toggle"]]
      [div ~a:[a_class ["ot-active"; "ot-up"]]
         [pcdata up_txt];
       div ~a:[a_class ["ot-inactive"; "ot-down"];
               a_onclick {{ fun _ -> %f T_Down }} ]
         [pcdata down_txt]]
  | T_Down ->
    div ~a:[a_class ["ot-toggle"]]
      [div ~a:[a_class ["ot-inactive"; "ot-up"];
               a_onclick {{ fun _ -> %f T_Up }} ]
         [pcdata up_txt];
       div ~a:[a_class ["ot-active"; "ot-down"]]
         [pcdata down_txt]]

let is_up = function
  | T_Up ->
    true
  | T_Down ->
    false

}}

{shared{

let make ?init_up:(init_up = false) ?up_txt ?down_txt () =
  let e, f =
    (if init_up then T_Up else T_Down) |>
    Eliom_csreact.SharedReact.S.create
  in
  r_node
    (Eliom_csreact.SharedReact.S.map
       (Eliom_lib.create_shared_value
          (display_toggle f ?up_txt ?down_txt)
          {{display_toggle %f ?up_txt:%up_txt ?down_txt:%down_txt}})
       e),
  Eliom_csreact.SharedReact.S.map
    (Eliom_lib.create_shared_value is_up {{ is_up }} )
    e

}}
