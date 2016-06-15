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

[%%shared open Eliom_content.Html ]
[%%shared open Eliom_content.Html.F ]

let%shared hcf ?(a=[]) ?(header=[]) ?(footer=[]) content =
  D.section
    ~a:(a_class ["ot-hcf"] :: (a :> Html_types.div_attrib attrib list))
    [ F.header ~a:[ a_class ["ot-hcf-header"] ] header
    ; div ~a:[ a_class ["ot-hcf-content"] ] content
    ; F.footer ~a:[ a_class ["ot-hcf-footer"] ] footer ]

let%client number_of_popups = ref 0

let%client scroll_pos = ref 0

let%client popup
    ?(a = [])
    ?close_button
    ?confirmation_onclose
    ?(onclose = fun () -> Lwt.return ())
    ?(setup_form=false) (*TODO*)
    ?(ios_scroll_pos_fix=true)
    gen_content =
  let a = (a :> Html_types.div_attrib attrib list) in
  let gen_content =
    (gen_content :> (unit -> unit Lwt.t) -> Html_types.div_content elt Lwt.t)
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
    decr number_of_popups;
    if !number_of_popups = 0 then begin
      html_ManipClass_remove "ot-with-popup";
      if ios_scroll_pos_fix then
        Dom_html.document##.body##.scrollTop := !scroll_pos
    end;
    let () = Eliom_lib.Option.iter Manip.removeSelf !popup in
    onclose ()
  in

  begin Eliom_client.onunload @@ fun () ->
    html_ManipClass_remove "ot-with-popup";
    None
  end;

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
  let a = (a :> Html_types.div_attrib attrib list) in
  ask_question
    ~a:(a_class [ "ot-popup-confirmation" ] :: a)
    ~header:question
    ~buttons:[ (yes, (fun () -> Lwt.return true) , ["ot-popup-yes"])
             ; (no , (fun () -> Lwt.return false), ["ot-popup-no"]) ] []


[%%client
class type form_element = object
  inherit Dom_html.element
  method tabIndex : int Js.prop
end
]

let%client try_focus x = match Dom_html.tagged x with
  | Dom_html.A        x -> x##focus
  | Dom_html.Input    x -> x##focus
  | Dom_html.Textarea x -> x##focus
  | Dom_html.Select   x -> x##focus
  | _ -> ()

let%client setup_form first second next_to_last last =
  begin Lwt.async @@ fun () ->
    let%lwt _ = Ot_nodeready.nodeready first in
    try_focus first;
    Lwt.return ()
  end;
  begin Lwt.async @@ fun () -> Lwt_js_events.focuses first @@ fun _ _ ->
    last##.tabIndex := 1;
    first##.tabIndex := 2;
    second##.tabIndex := 3;
    Lwt.return ()
  end;
  begin Lwt.async @@ fun () -> Lwt_js_events.blurs first @@ fun _ _ ->
    first##.tabIndex := 0;
    second##.tabIndex := 0;
    last##.tabIndex := 0;
    Lwt.return ()
  end;
  begin Lwt.async @@ fun () -> Lwt_js_events.focuses last @@ fun _ _ ->
    next_to_last##.tabIndex := 1;
    last##.tabIndex := 2;
    first##.tabIndex := 3;
    Lwt.return ()
  end;
  begin Lwt.async @@ fun () -> Lwt_js_events.blurs last @@ fun _ _ ->
    next_to_last##.tabIndex := 0;
    last##.tabIndex := 0;
    first##.tabIndex := 0;
    Lwt.return ()
  end

let%client coerce_to_form_element x = let x = Dom_html.element x in
  match Js.to_string x##.tagName with
  | "a"        -> (Dom_html.CoerceTo.a x :>        form_element Js.t Js.opt)
  (* | "link"     -> (Dom_html.CoerceTo.link x :>     <tabIndex: int Js.prop> Js.t Js.opt) *)
  (*TODO: buttons will work once we remove the focus requirement *)
  (* | "button"   -> (Dom_html.CoerceTo.button x :>   form_element Js.t Js.opt) *)
  | "input"    -> (Dom_html.CoerceTo.input x :>    form_element Js.t Js.opt)
  | "select"   -> (Dom_html.CoerceTo.select x :>   form_element Js.t Js.opt)
  | "textarea" -> (Dom_html.CoerceTo.textarea x :> form_element Js.t Js.opt)
  (* | "menuitem" -> (Dom_html.CoerceTo.menuitem x :> <tabIndex: int Js.prop> Js.t Js.opt) *)
  | _ -> Js.Opt.empty

let%client coerce_to_tabIndexable x = let x = Dom_html.element x in
  match Js.to_string x##.tagName with
  | "a"        -> (Dom_html.CoerceTo.a x :>        <tabIndex: int Js.prop> Js.t Js.opt)
  (* | "link"     -> (Dom_html.CoerceTo.link x :>     <tabIndex: int Js.prop> Js.t Js.opt) *)
  | "button"   -> (Dom_html.CoerceTo.button x :>   <tabIndex: int Js.prop> Js.t Js.opt)
  | "input"    -> (Dom_html.CoerceTo.input x :>    <tabIndex: int Js.prop> Js.t Js.opt)
  | "select"   -> (Dom_html.CoerceTo.select x :>   <tabIndex: int Js.prop> Js.t Js.opt)
  | "textarea" -> (Dom_html.CoerceTo.textarea x :> <tabIndex: int Js.prop> Js.t Js.opt)
  (* | "menuitem" -> (Dom_html.CoerceTo.menuitem x :> <tabIndex: int Js.prop> Js.t Js.opt) *)
  | _ -> Js.Opt.empty

(* TODO: what if there are only one or two tabIndexable elements? *)
let%client setup_form_auto form =
  let descendants = ref @@
    Dom.list_of_nodeList @@ form##getElementsByTagName (Js.string "*") in

  let rec first_tabIndex_element xs = match xs with
    | [] -> failwith "could not find valid form element" (*TODO: exception*)
    | x::xs -> Js.Opt.case (coerce_to_tabIndexable x)
                 (fun () -> first_tabIndex_element xs)
                 (fun x -> x)
  in

  let first_form_element = failwith "muh" in
    (*TODO: same but delete! *) 

  let first = first_form_element !descendants in
  let second = first_tabIndex_element !descendants in
  descendants := List.rev !descendants;
  let last = first_form_element !descendants in
  let next_to_last = first_tabIndex_element !descendants in

  setup_form first second next_to_last last
