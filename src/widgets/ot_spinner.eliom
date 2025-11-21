(* Ocsigen Toolkit
 * http://www.ocsigen.org/ocsigen-toolkit
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

open%client Js_of_ocaml

[%%shared open Eliom_content.Html]
[%%shared open Lwt.Syntax]
[%%shared open Eliom_content.Html.F]
[%%client open Eliom_shared]

let%shared default_fail_fun e =
  [ (if Eliom_config.get_debugmode ()
     then em [txt (Printexc.to_string e)]
     else
       let e = Printexc.to_string e in
       ignore
         [%client
           (Console.console##error
              (Js.string ("Ot_spinner content failed with " ^ ~%e))
            : unit)];
       em ~a:[a_class ["ot-icon-error"]] []) ]

let%shared default_fail_ref :
  (exn -> Html_types.div_content Eliom_content.Html.elt list) ref
  =
  ref default_fail_fun

let%shared default_fail e =
  (!default_fail_ref e
    : Html_types.div_content Eliom_content.Html.elt list
    :> [< Html_types.div_content] Eliom_content.Html.elt list)

let%client set_default_fail f =
  default_fail_ref :=
    (f
      : exn -> [< Html_types.div_content] Eliom_content.Html.elt list
      :> exn -> Html_types.div_content Eliom_content.Html.elt list)

let%server with_spinner ?(a = []) ?spinner:_ ?fail thread =
  let a = (a :> Html_types.div_attrib attrib list) in
  let fail =
    ((match fail with
     | Some fail -> (fail :> exn -> Html_types.div_content elt list Lwt.t)
     | None -> fun e -> Lwt.return (default_fail e))
      :> exn -> Html_types.div_content elt list Lwt.t)
  in
  let* v =
    Lwt.catch
      (fun () ->
         let* v = thread in
         Lwt.return (v :> Html_types.div_content_fun F.elt list))
      (fun e ->
         let* v = fail e in
         Lwt.return (v :> Html_types.div_content_fun F.elt list))
  in
  Lwt.return (D.div ~a:(a_class ["ot-spinner"] :: a) v)

[%%client
let num_active_spinners, set_num_active_spinners = React.S.create 0
let onloaded, set_onloaded = React.E.create ()

(* Make sure the signal is not destroyed indirectly
   by a call to React.E.stop *)
let _ = ignore (React.E.map (fun _ -> ()) onloaded)

let _ =
  Ot_lib.onloads @@ fun () ->
  if React.S.value num_active_spinners = 0 then set_onloaded ()

let inc_active_spinners () =
  set_num_active_spinners @@ (React.S.value num_active_spinners + 1)

let dec_active_spinners () =
  set_num_active_spinners @@ (React.S.value num_active_spinners - 1);
  if React.S.value num_active_spinners = 0 then set_onloaded ()

let cl_spinning = "ot-icon-animation-spinning"
let cl_spinner = "ot-icon-spinner"

let replace_content ?fail elt thread =
  let fail =
    match fail with
    | Some fail ->
        (fail
          : exn -> [< Html_types.div_content] Eliom_content.Html.elt list Lwt.t
          :> exn -> Html_types.div_content Eliom_content.Html.elt list Lwt.t)
    | None -> fun e -> Lwt.return (default_fail e)
  in
  inc_active_spinners ();
  Manip.replaceChildren elt [];
  Manip.Class.add elt cl_spinning;
  Manip.Class.add elt cl_spinner;
  let* new_content = Lwt.catch (fun () -> thread) (fun e -> fail e) in
  Manip.replaceChildren elt new_content;
  Manip.Class.remove elt cl_spinning;
  Manip.Class.remove elt cl_spinner;
  dec_active_spinners ();
  Lwt.return_unit

module Make (A : sig
    type +'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val bind2 : 'a t -> ('a -> 'b Lwt.t) -> 'b Lwt.t
    val return : 'a -> 'a t
  end) =
struct
  let with_spinner ?(a = []) ?spinner ?fail thread =
    let a = (a :> Html_types.div_attrib attrib list) in
    let fail =
      match fail with
      | Some fail ->
          (fail
            : exn -> [< Html_types.div_content] elt list A.t
            :> exn -> Html_types.div_content elt list A.t)
      | None -> fun e -> A.return (default_fail e)
    in
    match Lwt.state thread with
    | Lwt.Return v -> A.return (D.div ~a:(a_class ["ot-spinner"] :: a) v)
    | Lwt.Sleep ->
        inc_active_spinners ();
        let cl = ["ot-spinner"] in
        let cl =
          if spinner = None then cl_spinner :: cl_spinning :: cl else cl
        in
        let d =
          D.div ~a:(a_class cl :: a)
            (match spinner with None -> [] | Some s -> s)
        in
        Lwt.async (fun () ->
          let* v =
            Lwt.catch
              (fun () ->
                 let* v = thread in
                 Lwt.return (v :> Html_types.div_content_fun F.elt list))
              (fun e ->
                 A.bind2 (fail e) (fun v ->
                   Lwt.return (v :> Html_types.div_content_fun F.elt list)))
          in
          Manip.replaceChildren d v;
          Manip.Class.remove d cl_spinning;
          Manip.Class.remove d cl_spinner;
          dec_active_spinners ();
          Lwt.return_unit);
        A.return d
    | Lwt.Fail e -> A.bind (fail e) (fun c -> A.return (D.div ~a c))
end

module N = Make (struct
    type +'a t = 'a

    let bind a f = f a
    let bind2 a f = f a
    let return a = a
  end)

module L = Make (struct
    include Lwt

    let bind2 = bind
  end)

let with_spinner_no_lwt = N.with_spinner
let with_spinner = L.with_spinner]
