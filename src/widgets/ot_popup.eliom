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

[%%shared open Eliom_content.Html]
[%%shared open Eliom_content.Html.F]

let%shared hcf ?(a=[]) ?(header=[]) ?(footer=[]) content =
  D.section
    ~a:(a_class ["ot-hcf"] :: (a :> Html_types.div_attrib attrib list))
    [ F.header ~a:[ a_class ["ot-hcf-header"] ] header
    ; div ~a:[ a_class ["ot-hcf-content"] ] content
    ; F.footer ~a:[ a_class ["ot-hcf-footer"] ] footer ]

let%client scroll_pos = ref 0

let%client popup
    ?(a = [])
    ?close_button
    ?confirmation_onclose
    ?(onclose = fun () -> Lwt.return_unit)
    ?(close_on_background_click=false)
    ?(ios_scroll_pos_fix=true)
    gen_content =
  let a = (a :> Html_types.div_attrib attrib list) in
  let gen_content =
    (gen_content :> (unit -> unit Lwt.t) -> Html_types.div_content elt Lwt.t)
  in
  let popup = ref None in

  let html = Js.Opt.to_option @@
    Js.Opt.map (Dom_html.CoerceTo.html Dom_html.document##.documentElement)
      Of_dom.of_html
  in
  let html_ManipClass_add cl = match html with
    | Some html -> Manip.Class.add html cl
    | None -> ()
  in
  let html_ManipClass_remove cl = match html with
    | Some html -> Manip.Class.remove html cl
    | None -> ()
  in

  if ios_scroll_pos_fix then scroll_pos := Dom_html.document##.body##.scrollTop;
  html_ManipClass_add "ot-with-popup";
  if ios_scroll_pos_fix then Dom_html.document##.body##.scrollTop := !scroll_pos;

  let do_close () =
    if (Dom_html.document##getElementsByClassName (Js.string "ot-popup"))
       ##.length = 1 then begin
      html_ManipClass_remove "ot-with-popup";
      if ios_scroll_pos_fix then
        Dom_html.document##.body##.scrollTop := !scroll_pos
    end;
    let () = Eliom_lib.Option.iter Manip.removeSelf !popup in
    onclose ()
  in

  Eliom_client.onunload (fun () -> html_ManipClass_remove "ot-with-popup");

  let close () =
    match confirmation_onclose with
    | None -> do_close ()
    | Some f ->
      match%lwt f ()
      with true -> do_close ()
         | false -> Lwt.return_unit
  in
  (* FIXME: use a list for gen_content return type *)
  let%lwt c = Ot_spinner.with_spinner ~a:[a_class ["ot-popup-content"]]
      (Lwt.map (fun x -> [x]) (gen_content do_close))
  in

  let content = [c] in
  let content = match close_button with
    | Some but ->
      button
        ~a:[ a_button_type `Button
           ; a_class ["ot-popup-close"]
           ; a_onclick (fun ev -> Lwt.async (fun () -> close ())) ] but
      :: content
    | None -> content in
  let pop = D.div ~a:[a_class ["ot-popup"]] content in
  let box = D.div ~a:(a_class ["ot-popup-background"] :: a) [ pop ] in

  if close_on_background_click then begin
    (* Close the popup when user clicks on background *)
    Lwt.async (fun () ->
      let box_dom = Eliom_content.Html.To_dom.of_element box in
      let%lwt event = Lwt_js_events.click box_dom in
      if event##.target = Js.some box_dom then
        close ()
      else
        Lwt.return_unit
    )
  end ;

  popup := Some box ;
  Manip.appendToBody box ;
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
                    let%lwt result = do_close () in
                    Lwt.wakeup w r;
                    Lwt.return result
                    ) )
           ; btn ) buttons in
         Lwt.return ( hcf ?a:a_hcf ~header ~footer:answers contents ) )
  in t

let%client confirm ?(a = []) question yes no =
  let a = (a :> Html_types.div_attrib attrib list) in
  ask_question
    ~a:(a_class [ "ot-popup-confirmation" ] :: a)
    ~header:question
    ~buttons:[ (yes, (fun () -> Lwt.return_true) , ["ot-popup-yes"])
             ; (no , (fun () -> Lwt.return_false), ["ot-popup-no"]) ] []
