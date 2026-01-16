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

open Js_of_ocaml

[%%client open Js_of_ocaml_eio]

open Eliom_content.Html
open Eliom_content.Html.F

class type tabbable = object
  inherit Dom_html.element
  method tabIndex : int Js.prop
end

let only_if_active' elt v = if Ot_style.invisible elt then None else Some v

let only_if_active elt v =
  if elt##.disabled = Js._true || Ot_style.invisible elt then None else Some v

let coerce_to_tabbable x =
  let x = Dom_html.element x in
  match Dom_html.tagged x with
  | Dom_html.A x -> only_if_active' x (x :> tabbable Js.t)
  (* | Dom_html.Link     x -> Some (x :> tabbable Js.t) *)
  | Dom_html.Button x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Input x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Select x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Textarea x -> only_if_active x (x :> tabbable Js.t)
  (* | Dom_html.Menuitem x -> Some (x :> tabbable Js.t) *)
  | _ -> None

(* https://www.w3.org/TR/html5/editing.html#sequential-focus-navigation-and-the-tabindex-attribute *)
let tabbable_elts_of elt =
  elt##querySelectorAll
    (Js.string
       "a[href],link[href],button,input:not([type=\"hidden\"]),select,textarea,[ot-form-focusable]")
  |> Dom.list_of_nodeList
  |> List.map coerce_to_tabbable
  |> List.fold_left (fun a -> function Some x -> x :: a | _ -> a) []
  |> List.rev

let setup_tabcycle (elts : #tabbable Js.t list) : unit =
  let rec fn n = function
    | [x] ->
        x##.tabIndex := n;
        (let open Eio_js_events in
         async @@ fun () ->
         focuses x @@ fun _ -> x##.tabIndex := 1);
        let open Eio_js_events in
        async @@ fun () ->
        blurs x @@ fun _ -> x##.tabIndex := n
    | hd :: tl ->
        hd##.tabIndex := n;
        fn (n + 1) tl
    | [] -> ()
  in
  fn 2 elts

let setup_tabcycle_auto x = setup_tabcycle (tabbable_elts_of x)
let focus_first = function x :: _ -> (Js.Unsafe.coerce x)##focus | [] -> ()

let prevent_tab elt =
  let save_and_set_tabindex idx elt =
    let old = elt##.tabIndex in
    elt##.tabIndex := idx;
    elt, old
  in
  let restore_tabindex (elt, i) = elt##.tabIndex := i in
  let elts = List.map (save_and_set_tabindex (-1)) (tabbable_elts_of elt) in
  fun () -> List.iter restore_tabindex elts

let setup_form element =
  let elts = tabbable_elts_of element in
  setup_tabcycle elts; focus_first elts
