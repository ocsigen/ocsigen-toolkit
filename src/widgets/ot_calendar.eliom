(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2015
 * Jerome Vouillon and Vasilis Papavasileiou
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

{shared{
open Eliom_content.Html5

let days = ["S"; "M"; "T"; "W"; "T"; "F"; "S"]

module A = CalendarLib.Date
}}

{client{

let timezone_offset =
  truncate (-. float (jsnew Js.date_now() ##getTimezoneOffset()) /. 60.)

let (>>!) = Js.Opt.iter

}} ;;

{server{ let timezone_offset = 0 }} ;;

{shared{

let user_tz = CalendarLib.Time_Zone.UTC_Plus timezone_offset

let to_local date =
  CalendarLib.(Time_Zone.on Calendar.from_gmt user_tz date)

let to_utc date =
  CalendarLib.(Time_Zone.on Calendar.to_gmt user_tz date)

let now () =
  to_local (CalendarLib.Calendar.now ())

let rec map_interval i j f =
  if i > j then
    []
  else
    f i :: map_interval (i + 1) j f

let rec iter_interval i j f =
  if i <= j then begin
    f i;
    iter_interval (i + 1) j f
  end

let build_table i_max j_max ~a ~thead ~f_a_row ~f_cell =
  let f i =
    let f j =
      let a, c = f_cell i j in
      D.td ~a c
    in
    D.tr ~a:(f_a_row i) (map_interval 0 j_max f)
  in
  D.table ~a ~thead (map_interval 0 i_max f)

let zeroth_displayed_day d =
  let fst_sun = A.(nth_weekday_of_month (year d) (month d) Sun 1) in
  if A.day_of_month fst_sun = 1 then
    fst_sun
  else
    A.prev fst_sun `Week

let rec build_calendar
    ?class_for_day:(class_for_day = false) day =
  let fst_sun = A.(nth_weekday_of_month (year day) (month day) Sun 1)
  and zero = zeroth_displayed_day day
  and prev_button =
    D.(span ~a:[a_class ["ot-c-prev-button"]] [pcdata "<"])
  and next_button =
    D.(span ~a:[a_class ["ot-c-next-button"]] [pcdata ">"])
  in
  let thead =
    D.(thead
         [tr [th ~a:[a_colspan 7; a_class ["ot-c-header"]]
                [prev_button;
                 CalendarLib.Printer.Date.sprint "%B %Y" fst_sun |>
                 pcdata;
                 next_button]];
          tr (List.map (fun d -> th [pcdata d]) days)])
  and f_cell i j =
    let d =
      let open CalendarLib.Calendar.Date in
      add zero (Period.day (j + 7 * i))
    in
    if A.month fst_sun = A.month d then
      let module C = CalendarLib.Calendar in
      (let today = A.today () in
       let classes =
         [if d < today then
            "ot-c-past"
          else if d = today then
            "ot-c-today"
          else
            "ot-c-future"]
       in
       let classes =
         if d = day && class_for_day then
           "ot-c-current" :: classes
         else
           classes
       in
       [D.a_class classes],
       [D.div [A.day_of_month d |> string_of_int |> D.pcdata]])
    else
      [], []
  and f_a_row i = [] in
  build_table 5 6 ~a:[D.a_class ["ot-c-table"]] ~thead ~f_cell ~f_a_row,
  prev_button, next_button

}} ;;

{client{

   let update_classes cal zero d =
     let rows = (To_dom.of_table cal)##rows in
     let f i =
       rows##item(i + 2) >>! fun r ->
       let cells = r##cells in
       let f j =
         cells##item(j) >>! fun c ->
         let d' =
           let open CalendarLib.Calendar.Date in
           j + 7 * i |> Period.day |> add zero
         in
         if d = d' then
           c##classList##add(Js.string "ot-c-current")
         else
           c##classList##remove(Js.string "ot-c-current")
       in
       iter_interval 0 6 f
     in
     iter_interval 0 4 f

let attach_events
       ?action
       ?click_non_highlighted:(click_non_highlighted = false)
       ?update
       d cal highlight =
     let rows = (To_dom.of_table cal)##rows in
     let fst_sun = A.(nth_weekday_of_month (year d) (month d) Sun 1)
     and zero = zeroth_displayed_day d in
     let mnth = A.month fst_sun in
     iter_interval 0 4 @@ fun i ->
     rows##item(i + 2) >>! fun r ->
     let cells = r##cells in
     iter_interval 0 6 @@ fun j ->
     cells##item(j) >>! fun c ->
     let d =
       let open CalendarLib.Calendar.Date in
       j + 7 * i |> Period.day |> add zero
     in
     if mnth = A.month d then
       let dom = CalendarLib.Calendar.Date.day_of_month d
       and m = CalendarLib.Calendar.Date.(month d |> int_of_month)
       and y = CalendarLib.Calendar.Date.year d in
       let action =
         match action with
         | Some action ->
           (fun _ r ->
              update_classes cal zero d;
              action y m dom;
              Lwt.return ())
         | None ->
           (fun _ r ->
              update_classes cal zero d;
              Lwt.return ())
       in
       let set_onclick () =
         let f () = Lwt_js_events.clicks c action in
         Lwt.async f
       in
       if List.exists ((=) dom) highlight then begin
         c##classList##add(Js.string "ot-c-highlight");
         set_onclick ()
       end else if click_non_highlighted then
         set_onclick ()
       else
         ()

let attach_events_lwt
    ?action ?click_non_highlighted d cal highlight =
  let f () =
    let m = A.(month d |> int_of_month)
    and y = A.year d in
    lwt highlight = highlight y m in
    attach_events ?action ?click_non_highlighted d cal highlight;
    Lwt.return ()
  in
  Lwt.async f

let attach_behavior
    ?highlight ?click_non_highlighted ?action
    d (cal, prev, next) f_d =
  (match highlight with
   | Some highlight ->
     attach_events_lwt ?click_non_highlighted ?action d cal highlight
   | None ->
     attach_events ?click_non_highlighted ?action d cal []);
  (* FIXME: these may fail to go to the previous month, e.g., for May
     31 (April and June have only 30) *)
  (To_dom.of_element prev)##onclick <-
    Dom_html.handler (fun _ -> f_d (A.prev d `Month); Js._false);
  (To_dom.of_element next)##onclick <-
    Dom_html.handler (fun _ -> f_d (A.next d `Month); Js._false)

let make :
  ?init : (int * int * int) ->
  ?highlight : (int -> int -> int list Lwt.t) ->
  ?click_non_highlighted : bool ->
  ?update : (int * int * int) React.E.t ->
  ?action : (int -> int -> int -> unit Lwt.t) ->
  unit ->
  [> Html5_types.table ] elt =
  fun ?init ?highlight ?click_non_highlighted ?update ?action () ->
    let init =
      A.(match init with
        | Some (y, m, d) ->
          make y m d
        | None ->
          today ())
    in
    let d, f_d = React.S.create init in
    let f d =
      let (cal, _, _) as c = build_calendar ~class_for_day:true d in
      attach_behavior ?highlight ?click_non_highlighted ?action d c f_d;
      cal
    in
    (match update with
     | Some update ->
       let f (y, m, d) =
         A.make y m d |> f_d;
         (match action with
          | Some action ->
            Lwt.async (fun () -> action y m d)
          | None ->
            ())
       in
       React.E.map f update |> ignore
     | None ->
       ());
    React.S.map f d |> R.node

}}

{server{

   let make :
     ?init : (int * int * int) ->
     ?highlight :
     (int -> int -> int list Lwt.t) Eliom_lib.client_value ->
     ?click_non_highlighted : bool ->
     ?update :
     (int * int * int) React.E.t Eliom_lib.client_value ->
     ?action :
     (int -> int -> int -> unit Lwt.t) Eliom_lib.client_value ->
     unit ->
     [> Html5_types.table ] elt =
     fun ?init ?highlight ?click_non_highlighted ?update ?action () ->
       {[> Html5_types.table ] elt{ make
            ?init:%init
            ?highlight:%highlight
            ?click_non_highlighted:%click_non_highlighted
            ?update:%update
            ?action:%action () }} |>
       C.node

 }} ;;

{shared{

let make_date_picker ?init ?update () =
  let init =
    match init with
    | Some init ->
      init
    | None ->
      let d = A.today () in
      A.(year d, month d |> int_of_month, day_of_month d)
  in
  let v, f =  Eliom_shared.React.S.create init in
  let action = {{ fun y m d -> %f (y, m, d); Lwt.return () }}
  and click_non_highlighted = true in
  let d = make ~init ~click_non_highlighted ?update ~action () in
  d, v

}}
