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

[%%shared.start] (* shared by default, override as necessary *)

open Eliom_content.Html

type dow = [`Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat]
type intl = {i_days : string list; i_months : string list; i_start : dow}

type button_labels =
  { b_prev_year : string
  ; b_prev_month : string
  ; b_next_month : string
  ; b_next_year : string }

[%%client.start]

open Js_of_ocaml
open Js_of_ocaml_lwt

let default_intl =
  { i_days = ["S"; "M"; "T"; "W"; "T"; "F"; "S"]
  ; i_months =
      [ "Jan"
      ; "Feb"
      ; "Mar"
      ; "Apr"
      ; "May"
      ; "Jun"
      ; "Jul"
      ; "Aug"
      ; "Sep"
      ; "Oct"
      ; "Nov"
      ; "Dec" ]
  ; i_start = `Sun }

type 'a period =
  {begin_p : 'a CalendarLib.Date.date; end_p : 'a CalendarLib.Date.date}

let default_period =
  { begin_p = CalendarLib.Date.lmake ~year:1 ()
  ; end_p = CalendarLib.Date.lmake ~year:3200 ~month:12 ~day:31 () }

let default_button_labels =
  { b_prev_year = "<<"
  ; b_prev_month = "<"
  ; b_next_month = ">"
  ; b_next_year = ">>" }

let calendarlib_of_dow =
  let open CalendarLib.Date in
  function
  | `Sun -> Sun
  | `Mon -> Mon
  | `Tue -> Tue
  | `Wed -> Wed
  | `Thu -> Thu
  | `Fri -> Fri
  | `Sat -> Sat

let int_of_dow = function
  | `Sun -> 0
  | `Mon -> 1
  | `Tue -> 2
  | `Wed -> 3
  | `Thu -> 4
  | `Fri -> 5
  | `Sat -> 6

let string_of_month intl_months = function
  | CalendarLib.Date.Jan -> List.nth intl_months 0
  | CalendarLib.Date.Feb -> List.nth intl_months 1
  | CalendarLib.Date.Mar -> List.nth intl_months 2
  | CalendarLib.Date.Apr -> List.nth intl_months 3
  | CalendarLib.Date.May -> List.nth intl_months 4
  | CalendarLib.Date.Jun -> List.nth intl_months 5
  | CalendarLib.Date.Jul -> List.nth intl_months 6
  | CalendarLib.Date.Aug -> List.nth intl_months 7
  | CalendarLib.Date.Sep -> List.nth intl_months 8
  | CalendarLib.Date.Oct -> List.nth intl_months 9
  | CalendarLib.Date.Nov -> List.nth intl_months 10
  | CalendarLib.Date.Dec -> List.nth intl_months 11

let int_of_month = function
  | CalendarLib.Date.Jan -> 1
  | CalendarLib.Date.Feb -> 2
  | CalendarLib.Date.Mar -> 3
  | CalendarLib.Date.Apr -> 4
  | CalendarLib.Date.May -> 5
  | CalendarLib.Date.Jun -> 6
  | CalendarLib.Date.Jul -> 7
  | CalendarLib.Date.Aug -> 8
  | CalendarLib.Date.Sep -> 9
  | CalendarLib.Date.Oct -> 10
  | CalendarLib.Date.Nov -> 11
  | CalendarLib.Date.Dec -> 12

let int_of_strmonth intl_months month =
  let rec aux i =
    try if month = List.nth intl_months i then i + 1 else aux (i + 1)
    with _ -> failwith "not_a_month"
  in
  aux 0

let rec rotate_list ?(acc = []) l i =
  if i <= 0
  then List.append l (List.rev acc)
  else
    match l with
    | h :: l -> rotate_list ~acc:(h :: acc) l (i - 1)
    | [] -> failwith "rotate_list"

let get_rotated_days {i_days; i_start} =
  if List.length i_days != 7
  then invalid_arg "intl.i_days"
  else rotate_list i_days (int_of_dow i_start)

let name_of_calendarlib_month {i_months} m =
  if List.length i_months != 12
  then invalid_arg "intl.i_months"
  else List.nth i_months (CalendarLib.Date.int_of_month m - 1)

let timezone_offset =
  truncate (-.float (new%js Js.date_now)##getTimezoneOffset /. 60.)

let ( >>! ) = Js.Opt.iter
let user_tz = CalendarLib.Time_Zone.UTC_Plus timezone_offset
let to_local date = CalendarLib.(Time_Zone.on Calendar.from_gmt user_tz date)
let to_utc date = CalendarLib.(Time_Zone.on Calendar.to_gmt user_tz date)
let now () = to_local (CalendarLib.Calendar.now ())

let rec map_interval i j f =
  if i > j then [] else f i :: map_interval (i + 1) j f

let rec iter_interval i j f =
  if i <= j
  then (
    f i;
    iter_interval (i + 1) j f)

let build_table i_max j_max ~a ~thead ~f_a_row ~f_cell =
  let f i =
    let f j =
      let a, c = f_cell i j in
      D.td ~a c
    in
    D.tr ~a:(f_a_row i) (map_interval 0 j_max f)
  in
  D.table ~a ~thead (map_interval 0 i_max f)

let fst_dow ~intl:{i_start} d =
  let open CalendarLib.Date in
  nth_weekday_of_month (year d) (month d) (calendarlib_of_dow i_start) 1

let zeroth_displayed_day ~intl d =
  let o = fst_dow ~intl d in
  if CalendarLib.Date.day_of_month o = 1
  then o
  else CalendarLib.Date.prev o `Week

let select_action ?(size = 1) selector action =
  let dom_select = Eliom_content.Html.To_dom.of_select selector in
  Lwt.async (fun () ->
      action dom_select (fun _ _ ->
          dom_select##.size := size;
          Lwt.return_unit))

let rec build_calendar ?prehilight
    ~button_labels:{b_prev_year; b_prev_month; b_next_month; b_next_year} ~intl
    ~period day
  =
  let today = CalendarLib.Date.today () in
  let fst_dow = fst_dow ~intl day
  and zero = zeroth_displayed_day ~intl day
  and prev_button =
    D.(span ~a:[a_class ["ot-c-prev-button"]] [txt b_prev_month])
  and next_button =
    D.(span ~a:[a_class ["ot-c-next-button"]] [txt b_next_month])
  and prev_year_button =
    D.(span ~a:[a_class ["ot-c-prev-year-button"]] [txt b_prev_year])
  and next_year_button =
    D.(span ~a:[a_class ["ot-c-next-year-button"]] [txt b_next_year])
  and select_month =
    let month = CalendarLib.Date.month today |> string_of_month intl.i_months in
    let open D in
    select
      ~a:[a_class ["ot-c-select-month"]]
      (intl.i_months
      |> List.map (fun m ->
             if month = m
             then option ~a:[a_value m; a_selected ()] (txt m)
             else option ~a:[a_value m] (txt m)))
  and select_year =
    let year = CalendarLib.Date.year today |> string_of_int in
    let open D in
    select
      ~a:[a_class ["ot-c-select-year"]]
      (List.init
         CalendarLib.Date.(year period.end_p - year period.begin_p + 1)
         (fun i ->
           let y = string_of_int (CalendarLib.Date.year period.end_p - i) in
           if year = y
           then option ~a:[a_value y; a_selected ()] (txt y)
           else option ~a:[a_value y] (txt y)))
  in
  let () =
    let open Lwt_js_events in
    let size = 10 in
    select_action select_year mousedowns ~size;
    select_action select_year changes;
    select_action select_year blurs;
    select_action select_month mousedowns ~size;
    select_action select_month changes;
    select_action select_month blurs
  in
  let thead =
    let open D in
    thead
      [ tr
          [ th
              ~a:[a_colspan 7; a_class ["ot-c-header"]]
              [ div
                  ~a:[a_class ["ot-c-pv-button"]]
                  [prev_year_button; prev_button]
              ; div ~a:[a_class ["ot-c-select"]] [select_month; select_year]
              ; div
                  ~a:[a_class ["ot-c-nx-button"]]
                  [next_button; next_year_button] ] ]
      ; tr (List.map (fun d -> th [txt d]) (get_rotated_days intl)) ]
  and f_cell i j =
    let d = CalendarLib.Calendar.Date.(add zero (Period.day (j + (7 * i)))) in
    if CalendarLib.Date.month fst_dow = CalendarLib.Date.month d
    then
      let module C = CalendarLib.Calendar in
      let today = CalendarLib.Date.today () in
      let classes =
        [ (if d < period.begin_p || d > period.end_p
          then "ot-c-out-period"
          else if d < today
          then "ot-c-past"
          else if d = today
          then "ot-c-today"
          else "ot-c-future") ]
      in
      let classes =
        match prehilight with
        | Some d' when d = d' -> "ot-c-current" :: classes
        | _ -> classes
      in
      ( [D.a_class classes]
      , [D.div [CalendarLib.Date.day_of_month d |> string_of_int |> D.txt]] )
    else [], []
  and f_a_row i = [] in
  ( build_table 5 6 ~a:[D.a_class ["ot-c-table"]] ~thead ~f_cell ~f_a_row
  , prev_button
  , next_button
  , select_month
  , select_year
  , prev_year_button
  , next_year_button )

let get_options selector =
  let options = selector##.options in
  let size = selector##.options##.length - 1 in
  let rec aux i acc =
    if i < 0
    then acc
    else
      aux (i - 1)
        (Js.Opt.case
           (options##item i)
           (fun _ -> acc)
           (fun e -> (e##.value, i) :: acc))
  in
  aux size []

let update_classes cal zero d =
  let rows = (To_dom.of_table cal)##.rows in
  let f i =
    rows ## (item (i + 2)) >>! fun r ->
    let cells = r##.cells in
    let f j =
      cells ## (item j) >>! fun c ->
      let d' =
        CalendarLib.Calendar.Date.(j + (7 * i) |> Period.day |> add zero)
      in
      if d = d'
      then c ##. classList ## (add (Js.string "ot-c-current"))
      else c ##. classList ## (remove (Js.string "ot-c-current"))
    in
    iter_interval 0 6 f
  in
  iter_interval 0 5 f

let in_period ?(begin_p = default_period.begin_p)
    ?(end_p = default_period.end_p) curr_date
  =
  curr_date >= begin_p && curr_date <= end_p

let in_period_coerced ?(begin_p = default_period.begin_p)
    ?(end_p = default_period.end_p) curr_date
  =
  curr_date >= (begin_p :> [`Month | `Year] CalendarLib.Date.date)
  && curr_date <= (end_p :> [`Month | `Year] CalendarLib.Date.date)

let attach_events ?action ?(click_non_highlighted = false) ?update ~intl ~period
    d cal highlight
  =
  let rows = (To_dom.of_table cal)##.rows in
  let fst_dow = fst_dow ~intl d and zero = zeroth_displayed_day ~intl d in
  let mnth = CalendarLib.Date.month fst_dow in
  iter_interval 0 5 @@ fun i ->
  rows ## (item (i + 2)) >>! fun r ->
  let cells = r##.cells in
  iter_interval 0 6 @@ fun j ->
  cells ## (item j) >>! fun c ->
  let d = CalendarLib.Calendar.Date.(j + (7 * i) |> Period.day |> add zero) in
  if mnth = CalendarLib.Date.month d
  then
    let dom = CalendarLib.Calendar.Date.day_of_month d
    and m = CalendarLib.Calendar.Date.(month d |> int_of_month)
    and y = CalendarLib.Calendar.Date.year d in
    let action =
      match action with
      | Some action ->
          fun _ r ->
            update_classes cal zero d;
            let%lwt _ = action y m dom in
            Lwt.return_unit
      | None -> fun _ r -> update_classes cal zero d; Lwt.return_unit
    in
    let set_onclick () =
      if in_period d ~begin_p:period.begin_p ~end_p:period.end_p
      then
        let f () = Lwt_js_events.clicks c action in
        Lwt.async f
      else Lwt.async (fun () -> Lwt.return_unit)
    in
    if List.exists (( = ) dom) highlight
    then (
      c ##. classList ## (add (Js.string "ot-c-highlight"));
      set_onclick ())
    else if click_non_highlighted
    then set_onclick ()
    else ()

let attach_events_lwt ?action ?click_non_highlighted ~intl ~period d cal
    highlight
  =
  let f () =
    let m = CalendarLib.Date.(month d |> int_of_month)
    and y = CalendarLib.Date.year d in
    let%lwt highlight = highlight y m in
    attach_events ?action ?click_non_highlighted ~intl ~period d cal highlight;
    Lwt.return_unit
  in
  Lwt.async f

let make_span_handler selector
    ((get_sig, set_sig) : int React.signal * (?step:React.step -> int -> unit))
    apply condi fun_handler
  =
  Dom_html.handler (fun _ ->
      if condi ()
      then (
        fun_handler ();
        set_sig (apply (React.S.value get_sig) 1);
        selector##.selectedIndex := React.S.value get_sig;
        Js._false)
      else Js._false)

let set_selected_index selector selector_value options
    ((get_sig, set_sig) : int React.signal * (?step:React.step -> int -> unit))
    ()
  =
  let value = selector_value () in
  try
    let index = List.assoc (Js.string value) options in
    set_sig index;
    selector##.selectedIndex := React.S.value get_sig
  with Not_found -> ()

let make_selector_handler change_index condi fun_handler =
  Dom_html.handler (fun _ ->
      if condi ()
      then (fun_handler (); change_index (); Js._false)
      else Js._false)

let attach_behavior ?highlight ?click_non_highlighted ?action ~intl ~period d
    (cal, prev, next, select_month, select_year, prev_year, next_year) f_d
  =
  (match highlight with
  | Some highlight ->
      attach_events_lwt ?click_non_highlighted ?action ~intl ~period d cal
        highlight
  | None -> attach_events ?click_non_highlighted ?action ~intl ~period d cal []);
  let s_y = To_dom.of_select select_year in
  let s_m = To_dom.of_select select_month in
  let s_y_v () = Js.to_string s_y##.value in
  let s_m_v () = Js.to_string s_m##.value in
  let options_year = get_options s_y in
  let options_month = get_options s_m in
  let valid_period condi () =
    in_period_coerced ~begin_p:period.begin_p ~end_p:period.end_p condi
  in
  let select_condi =
    valid_period
      (CalendarLib.Date.make_year_month
         (s_y_v () |> int_of_string)
         (s_m_v () |> int_of_strmonth intl.i_months))
  in
  let select_handler () =
    f_d
      (CalendarLib.Date.make_year_month
         (s_y_v () |> int_of_string)
         (s_m_v () |> int_of_strmonth intl.i_months))
  in
  let sig_year =
    React.S.create (CalendarLib.Date.year period.end_p - CalendarLib.Date.year d)
  in
  let sig_month =
    React.S.create ((CalendarLib.Date.month d |> int_of_month) - 1)
  in
  s_y##.selectedIndex := React.S.value (fst sig_year);
  s_m##.selectedIndex := React.S.value (fst sig_month);
  s_y##.onchange :=
    make_selector_handler
      (set_selected_index s_y s_y_v options_year sig_year)
      select_condi select_handler;
  s_m##.onchange :=
    make_selector_handler
      (set_selected_index s_m s_m_v options_month sig_month)
      select_condi select_handler;
  (To_dom.of_element prev)##.onclick
  := make_span_handler s_m sig_month ( - )
       (valid_period (CalendarLib.Date.prev d `Month))
       (fun () -> f_d (CalendarLib.Date.prev d `Month));
  (To_dom.of_element next)##.onclick
  := make_span_handler s_m sig_month ( + )
       (valid_period (CalendarLib.Date.next d `Month))
       (fun () -> f_d (CalendarLib.Date.next d `Month));
  (To_dom.of_element prev_year)##.onclick
  := make_span_handler s_y sig_year ( - )
       (valid_period (CalendarLib.Date.prev d `Year))
       (fun () -> f_d (CalendarLib.Date.prev d `Year));
  (To_dom.of_element next_year)##.onclick
  := make_span_handler s_y sig_year ( + )
       (valid_period (CalendarLib.Date.next d `Year))
       (fun () -> f_d (CalendarLib.Date.next d `Year))

let make
    :  ?init:int * int * int -> ?highlight:(int -> int -> int list Lwt.t)
    -> ?click_non_highlighted:bool -> ?update:(int * int * int) React.E.t
    -> ?action:(int -> int -> int -> unit Lwt.t)
    -> ?period:
         CalendarLib.Date.field CalendarLib.Date.date
         * CalendarLib.Date.field CalendarLib.Date.date
    -> ?button_labels:button_labels -> ?intl:intl -> unit
    -> [> Html_types.table] elt
  =
 fun ?init ?highlight ?click_non_highlighted ?update ?action
     ?(period = default_period.begin_p, default_period.end_p)
     ?(button_labels = default_button_labels) ?(intl = default_intl) () ->
  let period = {begin_p = fst period; end_p = snd period} in
  let init, init_ym =
    let y, m, d =
      match init with
      | Some x -> x
      | None ->
          let a = CalendarLib.Date.today () in
          CalendarLib.Date.(year a, int_of_month (month a), day_of_month a)
    in
    CalendarLib.Date.(make y m d, make_year_month y m)
  in
  CalendarLib.Printer.month_name := name_of_calendarlib_month intl;
  let d_ym, f_d_ym = React.S.create init_ym in
  let f d_ym =
    let ((cal, _, _, _, _, _, _) as c) =
      build_calendar ~intl ~button_labels ~prehilight:init ~period d_ym
    in
    attach_behavior ?highlight ?click_non_highlighted ?action ~intl ~period d_ym
      c f_d_ym;
    cal
  in
  (match update with
  | Some update ->
      let f (y, m, d) =
        CalendarLib.Date.make_year_month y m |> f_d_ym;
        match action with
        | Some action -> Lwt.async (fun () -> action y m d)
        | None -> ()
      in
      React.E.map f update |> ignore
  | None -> ());
  React.S.map f d_ym |> R.node

let%server make
    :  ?init:int * int * int
    -> ?highlight:(int -> int -> int list Lwt.t) Eliom_client_value.t
    -> ?click_non_highlighted:bool
    -> ?update:(int * int * int) React.E.t Eliom_client_value.t
    -> ?action:(int -> int -> int -> unit Lwt.t) Eliom_client_value.t
    -> ?period:
         CalendarLib.Date.field CalendarLib.Date.date
         * CalendarLib.Date.field CalendarLib.Date.date
    -> ?button_labels:button_labels -> ?intl:intl -> unit
    -> [> Html_types.table] elt
  =
 fun ?init ?highlight ?click_non_highlighted ?update ?action ?period
     ?button_labels ?intl () ->
  C.node
    [%client
      (make ?init:~%init ?highlight:~%highlight
         ?click_non_highlighted:~%click_non_highlighted ?update:~%update
         ?action:~%action ?period:~%period ?intl:~%intl
         ?button_labels:~%button_labels ()
        : [> Html_types.table] elt)]

let%shared make_date_picker ?init ?update ?button_labels ?intl ?period () =
  let init =
    match init with
    | Some init -> init
    | None ->
        let d = CalendarLib.Date.today () in
        CalendarLib.Date.(year d, month d |> int_of_month, day_of_month d)
  in
  let v, f = Eliom_shared.React.S.create init in
  let action =
    [%client
      fun y m d ->
        ~%f (y, m, d);
        Lwt.return_unit]
  and click_non_highlighted = true in
  let d =
    make ~init ~click_non_highlighted ?update ?button_labels ?intl ?period
      ~action ()
  in
  d, v
