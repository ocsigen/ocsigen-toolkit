(* Eliom-base-app
 * http://www.ocsigen.org/eliom-base-app
 *
 * Copyright (C) 2014
 *      Vincent Balat
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

[%%shared open Eliom_content.Html5 ]
[%%shared open Eliom_content.Html5.F ]

let%shared default_fail e =
  Lwt.return [
    if Eliom_config.get_debugmode ()
    then em [ pcdata (Printexc.to_string e) ]
    else em ~a:[ a_class ["ot-spn-error"] ] [ pcdata (Printexc.to_string e) ] ]

let%server with_spinner ?a ?(fail=default_fail) thread =
  let%lwt v = try%lwt
      let%lwt v = thread in
      Lwt.return
        (v :> Html5_types.div_content_fun F.elt list)
    with e ->
      let%lwt v = fail e in
      Lwt.return
        (v :> Html5_types.div_content_fun F.elt list)
  in
  Lwt.return (D.div ?a v)

let%client with_spinner ?(a = []) ?(fail=default_fail) thread =
  match Lwt.state thread with
  | Lwt.Return v -> Lwt.return (D.div ~a v)
  | Lwt.Sleep ->
    let loading = "ot-spn-spinning" in
    let d = D.div ~a:(a_class [ loading ] :: a) [] in
    Lwt.async
      (fun () ->
         let%lwt v = try%lwt
             let%lwt v = thread in
             Lwt.return
               (v :> Html5_types.div_content_fun F.elt list)
           with e ->
             let%lwt v = fail e in
             Lwt.return
               (v :> Html5_types.div_content_fun F.elt list)
         in
         Manip.replaceChildren d v ;
         Manip.Class.remove d loading ;
         Lwt.return () ) ;
    Lwt.return d
  | Lwt.Fail e -> let%lwt c = fail e in Lwt.return (D.div ~a c)
