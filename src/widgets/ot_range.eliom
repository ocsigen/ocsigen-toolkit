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
module Html5 = Eliom_content.Html5
open Html5.F
}}

{shared{

type t = int * int * string array option

type irs = int React.signal

type irf = ?step:React.step -> int -> unit

type irp = irs * irf

}} ;;

{client{

    let display_aux (_, _, a) v =
      let v =
        match a with
        | Some a ->
          a.(v)
        | None ->
          string_of_int v
      and a = [a_class ["ot-r-value"]] in
      div ~a [pcdata v]

let go_up (lb, ub, a) ((r, f) : irp) =
  let v = React.S.value r in
  assert (v <= ub - 1);
  f (if v = ub - 1 then lb else v + 1)

let go_down (lb, ub, a) ((r, f) : irp) =
  let v = React.S.value r in
  assert (v >= lb);
  f (if v = lb then ub - 1 else v - 1)

}} ;;

{shared{

    let display
        ?txt_up:(txt_up = "up")
        ?txt_down:(txt_down = "down")
        (e : t) r =
      div ~a:[a_class ["ot-range"]]
        [div ~a:[a_class ["ot-r-up"];
                 a_onclick {{ fun _ -> go_up %e %r }}]
           [pcdata txt_up];
         Eliom_content.Html5.C.node
           {{ Eliom_content.Html5.R.node
                (React.S.map
                   (display_aux %e)
                   (fst %r)) }};
         div ~a:[a_class ["ot-r-down"];
                 a_onclick {{ fun _ -> go_down %e %r }}]
           [pcdata txt_down]]

let make ?txt_up ?txt_down ?f ?lb:(lb = 0) ub =
  assert (ub > lb);
  let r = {irp{ React.S.create %lb }}
  and a =
    match f with
    | Some f ->
      let f i = f (i + lb) in
      Some (Array.init (ub - lb) f)
    | None ->
      None
  in
  display ?txt_up ?txt_down (lb, ub, a) r

}}
