{shared{
module Html5 = Eliom_content.Html5
open Html5.F
}}

{shared{

    let days = ["S"; "M"; "T"; "W"; "T"; "F"; "S"]

type handler =
  ((int -> int -> int list Lwt.t) Eliom_lib.client_value *
   (int -> unit) Eliom_lib.client_value)

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
      td ~a c
    in
    tr ~a:(f_a_row i) (map_interval 0 j_max f)
  in
  Html5.D.table ~a ~thead (map_interval 0 i_max f)

let rec build_calendar day =
  let module D = CalendarLib.Date in
  let fst_sun =
    D.nth_weekday_of_month (D.year day) (D.month day) D.Sun 1 in
  let zero = D.prev fst_sun `Week
  and prev_button =
    let a = [a_class ["ot-c-prev-button"]] in
    Html5.D.span ~a [pcdata "previous"]
  and next_button =
    let a = [a_class ["ot-c-next-button"]] in
    Html5.D.span ~a [pcdata "next"] in
  let thead =
    thead
      [tr [th ~a:[a_colspan 7; a_class ["ot-c-header"]]
             [CalendarLib.Printer.Date.sprint "%B %Y" fst_sun |>
              pcdata;
              prev_button;
              next_button]];
       tr (List.map (fun d -> th [pcdata d]) days)]
  and f_cell i j =
    let d =
      let open CalendarLib.Calendar.Date in
      add zero (Period.day (j + 7 * i))
    in
    if D.month fst_sun = D.month d then
      let module C = CalendarLib.Calendar in
      (if d = CalendarLib.Date.today () then
         [a_class ["ot-c-today"]]
       else
         []),
      [div [D.day_of_month d |> string_of_int |> pcdata]]
    else
      [], []
  and f_a_row i = [] in
  build_table 5 6 ~a:[a_class ["ot-c-table"]] ~thead ~f_cell ~f_a_row,
  prev_button, next_button

}} ;;

{client{

    let attach_events d cal events act =
      let module D = CalendarLib.Date in
      let rows = (Html5.To_dom.of_table cal)##rows in
      let fst_sun =
        D.nth_weekday_of_month (D.year d) (D.month d) D.Sun 1
      in
      let zero = D.prev fst_sun `Week in
      let f i =
        rows##item(i + 2) >>! fun r ->
        let cells = r##cells in
        let f j =
          cells##item(j) >>! fun c ->
          let d =
            let open CalendarLib.Calendar.Date in
            j + 7 * i |> Period.day |> add zero
          in
          let dom = CalendarLib.Calendar.Date.day_of_month d in
          if List.exists ((=) dom) events then
            (c##classList##add(Js.string "ot-c-event");
             c##onclick <-
               let f _ = act dom; Js._false in
               Dom_html.handler f)
          else
            ()
        in
        iter_interval 0 6 f
      in
      iter_interval 0 4 f

let attach_events_lwt d ~handler:(get, act) cal =
  let f () =
    let m = CalendarLib.Date.(month d |> int_of_month)
    and y = CalendarLib.Date.year d in
    let open Lwt.Infix in
    get m y >>= fun events ->
    attach_events d cal events act; Lwt.return ()
  in
  Lwt.async f

let rec attach_behavior ?handler d (cal, prev, next) =
  let update d =
    let (cal', _, _) as c = build_calendar d in
    attach_behavior ?handler d c;
    let cal = Html5.To_dom.of_element cal in
    let cal' = Html5.To_dom.of_element cal' in
    (cal##parentNode >>! fun p -> Dom.replaceChild p cal' cal);
    Js._false
  in
  (match handler with
   | Some handler ->
     attach_events_lwt ~handler d cal
   | None ->
     ());
  let module D = CalendarLib.Date in
  (* FIXME: these may fail to go to the previous month, e.g., for May
     31 (April and June have only 30) *)
  (Html5.To_dom.of_element prev)##onclick <-
    Dom_html.handler (fun _ -> update (D.prev d `Month));
  (Html5.To_dom.of_element next)##onclick <-
    Dom_html.handler (fun _ -> update (D.next d `Month))

}}

{shared{

    let make ?handler () =
      let today = CalendarLib.Date.today () in
      let (cal, _, _) as c = build_calendar today in
      ignore {unit{
          attach_behavior ?handler:%handler %today %c }};
      cal

}}
