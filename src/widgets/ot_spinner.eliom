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

[%%shared open Eliom_content.Html ]
[%%shared open Eliom_content.Html.F ]
[%%client open Eliom_shared ]

let%shared default_fail e =
  [
    if Eliom_config.get_debugmode ()
    then em [ pcdata (Printexc.to_string e) ]
    else em ~a:[ a_class ["ot-icon-question"] ]
        [ pcdata (Printexc.to_string e) ] ]

let%server with_spinner ?(a = []) ?fail thread =
  let a = (a :> Html_types.div_attrib attrib list) in
  let fail =
    ((match fail with
       | Some fail -> (fail :> exn -> Html_types.div_content elt list Lwt.t)
       | None      -> (fun e -> Lwt.return (default_fail e)))
     :> exn -> Html_types.div_content elt list Lwt.t)
  in
  let%lwt v = try%lwt
      let%lwt v = thread in
      Lwt.return
        (v :> Html_types.div_content_fun F.elt list)
    with e ->
      let%lwt v = fail e in
      Lwt.return
        (v :> Html_types.div_content_fun F.elt list)
  in
  Lwt.return (D.div ~a:(a_class ["ot-spinner"] :: a) v)

[%%client

let num_active_spinners, set_num_active_spinners = React.S.create 0
let onloaded, set_onloaded = React.E.create ()
(* Make sure the signal is not destroyed indirectly
   by a call to React.E.stop *)
let _ = ignore (React.E.map (fun _ -> ()) onloaded)
let _ = Ot_lib.onloads @@ fun () ->
  if React.S.value num_active_spinners = 0 then set_onloaded ()
let when_loaded action = if React.S.value num_active_spinners = 0
  then ignore @@ action ()
  else ignore @@ React.E.once @@ React.E.map action onloaded
let inc_active_spinners () =
  set_num_active_spinners @@ React.S.value num_active_spinners + 1
let dec_active_spinners () =
  set_num_active_spinners @@ React.S.value num_active_spinners - 1;
  if React.S.value num_active_spinners = 0 then set_onloaded ()

module Make(A : sig
    type +'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val bind2 : 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
    val return : 'a -> 'a t
  end) = struct

  let with_spinner ?(a = []) ?fail thread =
    let a = (a :> Html_types.div_attrib attrib list) in
    let fail =
      match fail with
      | Some fail -> (fail : exn -> [< Html_types.div_content ] elt list A.t
                      :> exn -> Html_types.div_content elt list A.t)
      | None      -> (fun e -> A.return (default_fail e))
    in
    match Lwt.state thread with
    | Lwt.Return v -> A.return (D.div ~a:(a_class ["ot-spinner"] :: a) v)
    | Lwt.Sleep ->
      let spinning = "ot-icon-animation-spinning" in
      let spinner = "ot-icon-spinner" in
      inc_active_spinners ();
      let d = D.div ~a:(a_class [ "ot-spinner" ; spinner ; spinning ] :: a) []
      in
      Lwt.async
        (fun () ->
           let%lwt v = try%lwt
               let%lwt v = thread in
               Lwt.return
                 (v :> Html_types.div_content_fun F.elt list)
             with e ->
               A.bind2 (fail e) (fun v ->
                 dec_active_spinners ();
                 (Lwt.return (v :> Html_types.div_content_fun F.elt list)))
           in
           Manip.replaceChildren d v ;
           Manip.Class.remove d spinning ;
           Manip.Class.remove d spinner ;
           dec_active_spinners ();
           Lwt.return () ) ;
      A.return d
    | Lwt.Fail e -> A.bind (fail e) (fun c -> A.return (D.div ~a c))
end

module N = Make(struct
    type +'a t = 'a
    let bind a f = f a
    let bind2 a f = f a
    let return a = a
  end)

module L = Make(struct include Lwt let bind2 = bind end)

let with_spinner_no_lwt = N.with_spinner
let with_spinner = L.with_spinner
]
