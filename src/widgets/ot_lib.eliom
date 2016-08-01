(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015-09
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

let%client onloads handler =
  let rec loop () = Eliom_client.onload @@ fun () -> handler (); loop () in loop ()

let%client onresizes handler =
  let thread = Lwt_js_events.onresizes handler in
  Eliom_client.onunload (fun () -> Lwt.cancel thread);
  thread

let%client window_scroll ?use_capture () =
  Lwt_js_events.make_event Dom_html.Event.scroll ?use_capture Dom_html.window

let%client window_scrolls ?(ios_html_scroll_hack = false) ?use_capture handler =
  let thread = if ios_html_scroll_hack
    then begin
      let rec loop () =
        let%lwt e =
          Lwt.pick (
            List.map
              (* We listen to several elements because scroll events are
                 not happening on the same element on every platform. *)
              (fun element -> Lwt_js_events.scroll ?use_capture element)
              [ (Dom_html.window :> Dom_html.eventTarget Js.t) ;
                (Dom_html.document##.documentElement :> Dom_html.eventTarget Js.t) ;
                (Dom_html.document##.body :> Dom_html.eventTarget Js.t) ]
          )
        in
        let continue = ref true in
        let w =
          try%lwt fst(Lwt.task())
          with Lwt.Canceled -> continue := false; Lwt.return_unit in
        let%lwt () = handler e w in
        if !continue then
          loop ()
        else
          Lwt.return_unit
      in
      loop ()
    end
    else Lwt_js_events.seq_loop window_scroll ?use_capture () handler
  in
  Eliom_client.onunload (fun () -> Lwt.cancel thread);
  thread

let%client rec in_ancestors ~elt ~ancestor =
  elt == (ancestor : Dom_html.element Js.t)
  || (not (elt == Dom_html.document##.body)
      &&
      Js.Opt.case (elt##.parentNode)
        (fun () -> false)
        (fun parent ->
           Js.Opt.case (Dom_html.CoerceTo.element parent)
             (fun () -> false)
             (fun elt -> in_ancestors ~elt ~ancestor)))

let%client rec click_outside ?use_capture elt =
  let%lwt ev = Lwt_js_events.click ?use_capture Dom_html.document in
  Js.Opt.case (ev##.target)
    (fun () -> click_outside ?use_capture elt)
    (fun target ->
       if in_ancestors ~elt:target ~ancestor:(elt :> Dom_html.element Js.t)
       then click_outside ?use_capture elt
       else Lwt.return ev)

[%%shared
module List = struct

  let iteri2 f l1 l2 =
    let rec aux i l1 l2 = match l1, l2 with
      | a::ll1, b::ll2 -> f i a b; aux (i+1) ll1 ll2
      | _ -> ()
    in
    aux 0 l1 l2

end
]
