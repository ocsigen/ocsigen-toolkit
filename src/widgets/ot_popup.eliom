(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015
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

let%shared hcf ?(a=[]) ?(header=[]) ?(footer=[]) content =
  D.section
    ~a:(a_class ["ot-hcf"] :: (a :> Html5_types.div_attrib attrib list))
    [ F.header ~a:[ a_class ["ot-hcf-header"] ] header
    ; div ~a:[ a_class ["ot-hcf-content"] ] content
    ; F.footer ~a:[ a_class ["ot-hcf-footer"] ] footer ]

let%client number_of_popups = ref 0

let%client popup
    ?(a = [])
    ?close_button
    ?confirmation_onclose
    ?(onclose = fun () -> Lwt.return ())
    gen_content =
  let a = (a :> Html5_types.div_attrib attrib list) in
  let gen_content =
    (gen_content :> (unit -> unit Lwt.t) -> Html5_types.div_content elt Lwt.t)
  in
  let popup = ref None in

  (let cl = (Js.string "ot-popup") in
   if !number_of_popups <> 0 &&
      (Dom_html.document##getElementsByClassName cl)##.length = 0 then
     (* Before we open a popup, if the number of popups is not 0
        and there's no opened popup, we must reset the popup counter.
        This situation may happen when the user clicks on a link inside
        a popup and loads a new page without resetting the whole environment. *)
     number_of_popups := 0);

  incr number_of_popups;

  let body = (Of_dom.of_body (Dom_html.document##.body)) in
  Manip.Class.add body "ot-with-popup";

  let do_close () =
    decr number_of_popups;
    if !number_of_popups = 0 then Manip.Class.remove body "ot-with-popup";
    let () = Eliom_lib.Option.iter Manip.removeSelf !popup in
    onclose ()
  in

  let close () =
    match confirmation_onclose with
    | None -> do_close ()
    | Some f ->
      match%lwt f ()
      with true -> do_close ()
         | false -> Lwt.return ()
  in
  (* FIXME: use a list for gen_content return type *)
  let%lwt c = Ot_spinner.with_spinner ~a:[a_class ["ot-popup-content"]]
      (Lwt.map (fun x -> [x]) (gen_content do_close))
  in
  let content = [c] in
  let content = match close_button with
    | Some but ->
        Form.button_no_value ~button_type:`Button
          ~a:[ a_class ["ot-popup-close"]
             ; a_onclick (fun ev -> Lwt.async (fun () -> close ())) ] but
        :: content
    | None -> content in
  let pop = D.div ~a:[a_class ["ot-popup"]] content in
  let box = D.div ~a:(a_class ["ot-popup-background"] :: a) [ pop ] in
  popup := Some box;
  Manip.appendToBody box;
  (* We must set the height explicitely, otherwise it does not
     resize correctly when there is an hcf inside. *)
  (* (To_dom.of_element pop)##style##height <- Js.string "999999px"; *)
  (* lwt () = Lwt_js_events.request_animation_frame () in *)
  (* (To_dom.of_element pop)##style##height <- *)
  (*   Js.string (Printf.sprintf "%dpx" (To_dom.of_element c)##offsetHeight) ; *)
  Lwt.return box

let%client ask_question ?a ?a_hcf ~header ~buttons contents =
  let t, w = Lwt.wait () in
  let%lwt _ =
    popup ?a
      (fun do_close ->
         let answers =
           List.map (fun (content, action, btn_class) ->
             let btn = D.Raw.button ~a:[ a_class btn_class ] content in
             (* Onlick, give t the selected value
                and close question popup. *)
             Lwt.async (fun () ->
               Lwt_js_events.clicks (To_dom.of_element btn)
                 (fun _ _ ->
                    let%lwt r = action () in
                    Lwt.wakeup w r ;
                    do_close () ) )
           ; btn ) buttons in
         Lwt.return ( hcf ?a:a_hcf ~header ~footer:answers contents ) )
  in t

let%client confirm ?(a = []) question yes no =
  let a = (a :> Html5_types.div_attrib attrib list) in
  ask_question
    ~a:(a_class [ "ot-popup-confirmation" ] :: a)
    ~header:question
    ~buttons:[ (yes, (fun () -> Lwt.return true) , ["ot-popup-yes"])
             ; (no , (fun () -> Lwt.return false), ["ot-popup-no"]) ] []
