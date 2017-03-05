(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015 BeSport, Julien Sagot
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

let rec node_in_document node =
  node == (Dom_html.document :> Dom.node Js.t) ||
  Js.Opt.case (node##.parentNode) (fun () -> false) node_in_document

let watched = ref []

let handler records observer =
  let changes = ref false in
  for i = 0 to records##.length - 1 do
    Js.Optdef.iter (Js.array_get records i)
      (fun r -> if r##.addedNodes##.length > 0 then changes := true)
  done;
  if !changes then begin
    let (ready, not_ready) =
      List.partition (fun (n, _) -> node_in_document n) !watched in
    watched := not_ready;
    if not_ready = [] then observer##disconnect;
    List.iter (fun (_, s) -> Lwt.wakeup s ()) ready
  end

let observer =
  new%js MutationObserver.mutationObserver(Js.wrap_callback handler)

let config =
  let cfg = MutationObserver.empty_mutation_observer_init () in
  cfg##.childList := true;
  cfg##.subtree := true;
  cfg

let nodeready node =
  let node = (node :> Dom.node Js.t) in
  if node_in_document node then Lwt.return_unit else begin
    let t, s = Lwt.wait () in
    if !watched = [] then observer##observe Dom_html.document config;
    watched := (node, s) :: !watched;
    t
  end
