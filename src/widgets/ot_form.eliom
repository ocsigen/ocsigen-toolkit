(* Ocsigen
 * http://www.ocsigen.org
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

[%%client.start]

open Eliom_content.Html
open Eliom_content.Html.F

class type tabbable = object
  inherit Dom_html.element
  method tabIndex : int Js.prop
end

let tabswitch one two =
  let focuses_one = begin Lwt_js_events.focuses one @@ fun _ _ ->
    one##.tabIndex := 1;
    two##.tabIndex := 2;
    Lwt.return_unit
  end in
  let blurs_one = begin Lwt_js_events.blurs one @@ fun _ _ ->
    one##.tabIndex := 0;
    two##.tabIndex := 0;
    Lwt.return_unit
  end in
  let focuses_two = begin Lwt_js_events.focuses two @@ fun _ _ ->
    one##.tabIndex := 2;
    two##.tabIndex := 1;
    Lwt.return_unit
  end in
  let blurs_two = begin Lwt_js_events.blurs two @@ fun _ _ ->
    one##.tabIndex := 0;
    two##.tabIndex := 0;
    Lwt.return_unit
  end in
  Lwt.join [focuses_one; blurs_one; focuses_two; blurs_two]

let tabcycle first second next_to_last last =
  let focuses_first = begin Lwt_js_events.focuses first @@ fun _ _ ->
    last##.tabIndex := 1;
    first##.tabIndex := 2;
    second##.tabIndex := 3;
    Lwt.return_unit
  end in
  let blurs_first = begin Lwt_js_events.blurs first @@ fun _ _ ->
    first##.tabIndex := 0;
    second##.tabIndex := 0;
    last##.tabIndex := 0;
    Lwt.return_unit
  end in
  let focuses_last = begin Lwt_js_events.focuses last @@ fun _ _ ->
    next_to_last##.tabIndex := 1;
    last##.tabIndex := 2;
    first##.tabIndex := 3;
    Lwt.return_unit
  end in
  let blurs_last = begin Lwt_js_events.blurs last @@ fun _ _ ->
    next_to_last##.tabIndex := 0;
    last##.tabIndex := 0;
    first##.tabIndex := 0;
    Lwt.return_unit
  end in
  Lwt.join [focuses_first; blurs_first; focuses_last; blurs_last]

let only_if_active' elt v =
  if Ot_style.invisible elt then None else Some v
let only_if_active elt v =
  if elt##.disabled = Js._true || Ot_style.invisible elt then None else Some v

let%shared rec list_of_opts = function
  | [] -> []
  | None :: xs -> list_of_opts xs
  | Some x :: xs -> x :: list_of_opts xs

let setup_tabcycle elts =
  begin match elts with
    | [one; two] -> (* We can't have a proper tab cycle with just two elements
                       but we can at least make TAB work (but not shift-TAB) *)
      tabswitch one two
    | one :: two :: three :: xs ->
      let last, next_to_last = match List.rev @@ two :: three :: xs with
        | last :: next_to_last :: _ -> last, next_to_last
        | _ -> failwith "Ot_popup.setup_tabcycle: can't happen"
      in
      tabcycle one two next_to_last last
    | _ -> Lwt.return_unit
  end

let coerce_to_tabbable x = let x = Dom_html.element x in
  match Dom_html.tagged x with
  | Dom_html.A        x -> only_if_active' x (x :> tabbable Js.t)
  (* | Dom_html.Link     x -> Some (x :> tabbable Js.t) *)
  | Dom_html.Button   x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Input    x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Select   x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Textarea x -> only_if_active x (x :> tabbable Js.t)
  (* | Dom_html.Menuitem x -> Some (x :> tabbable Js.t) *)
  | _ -> None

let tabbable_elts_of elt = list_of_opts @@ List.map coerce_to_tabbable @@
  Dom.list_of_nodeList @@ elt##getElementsByTagName (Js.string "*")

(* TODO: what if there are only one or two form elements? *)
let setup_tabcycle_auto form = setup_tabcycle @@ tabbable_elts_of form

let focussable x =
  let do_it elt focus = fun () -> Lwt.async @@ fun () ->
    let%lwt _ = Ot_nodeready.nodeready elt in
    focus ();
    Lwt.return_unit
  in
  match Dom_html.tagged x with
  | Dom_html.A        x -> only_if_active' x (do_it x @@ fun () -> x##focus)
  | Dom_html.Input    x -> only_if_active x (do_it x @@ fun () -> x##focus)
  | Dom_html.Textarea x -> only_if_active x (do_it x @@ fun () -> x##focus)
  | Dom_html.Select   x -> only_if_active x (do_it x @@ fun () -> x##focus)
  (* NOTE: buttons are focussable in most browser; but not in the specs! *)
  | Dom_html.Button   x -> only_if_active x (do_it x @@ fun () -> (Js.Unsafe.coerce x)##focus)
  | _ -> None

let focus_first_focussable elts =
  match list_of_opts @@ List.map focussable elts with
  | focus :: _ -> focus ()
  | [] -> ()

let setup_form ~trigger ?(modal=true) element =
  let%lwt () = Ot_nodeready.nodeready element in
  let () =  match trigger with
    | `OnNodeReady ->
      let elts = tabbable_elts_of element in
      setup_tabcycle elts ;
      focus_first_focussable elts
    | `OnSignal s ->
      let thread = ref None in
      let cancel () = match !thread with
        | None -> ()
        | Some t -> Lwt.cancel t
      in
      let stopper =
        React.S.map
          (function
            | true ->
              let elts = tabbable_elts_of element in
              thread := Some (setup_tabcycle elts) ;
              focus_first_focussable elts
            | false ->
              let elts = tabbable_elts_of element in
              cancel () ;
              List.iter (fun elt -> elt##.tabIndex := 0) elts)
          s
      in
      Eliom_client.onunload @@ fun () ->
      cancel () ;
      React.S.stop stopper
  in
  if modal
  then
    let all_tabbable_elts = tabbable_elts_of Dom_html.document in
    let not_in_form elt = not @@ Ot_lib.in_ancestors
        ~elt:(elt :> Dom_html.element Js.t)
        ~ancestor:(Dom_html.element element) in
    let extern = List.filter not_in_form all_tabbable_elts in
    let save_tabindex_and_disable_elt elt =
      let old_tabIndex = elt##.tabIndex in
      elt##.tabIndex := -1 ;
      (elt, old_tabIndex)
    in
    Lwt.return_some @@ fun () ->
    List.iter
      (fun (elt, i) -> elt##.tabIndex := i)
      (List.map save_tabindex_and_disable_elt extern)
  else Lwt.return_none

let resetup_form_signal () =
  let signal, set_signal = React.S.create true in
  let resetup_form () =
    let%lwt _ = Lwt_js.sleep 0.1 in (* wait until formular has been updated *)
    set_signal false;
    set_signal true;
    Lwt.return_unit
  in
  (`OnSignal signal, resetup_form)
