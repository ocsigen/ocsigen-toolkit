{shared{
module Html5 = Eliom_content.Html5
open Html5.F
}}

{shared{

    let days = ["S"; "M"; "T"; "W"; "T"; "F"; "S"]

type 'a event = CalendarLib.Calendar.t * CalendarLib.Calendar.t * 'a

}}

{client{

let timezone_offset =
  truncate (-. float (jsnew Js.date_now() ##getTimezoneOffset()) /. 60.)

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

let calendar_range day =
  let open CalendarLib in
  let month = Date.(nth_weekday_of_month (year day) (month day) Sun 1) in
  let start = Date.prev month `Week in
  let midnight = Time.midnight () in
  Calendar.create start midnight,
  Calendar.create (Date.add start (Date.Period.day 42)) midnight

let build_table i_max j_max ~a ~thead ~f_a_row ~f_cell =
  let f i =
    let f j =
      let a, c = f_cell i j in
      td ~a c
    in
    tr ~a:(f_a_row i) (map_interval 0 j_max f)
  in
  Html5.D.table ~a ~thead (map_interval 0 i_max f)

let rec build_calendar ?events:(events = []) day =
  let now = to_utc (now ()) in
  let module D = CalendarLib.Date in
  let month =
    D.nth_weekday_of_month (D.year day) (D.month day) D.Sun 1 in
  let start =
    D.prev month `Week in
  let midnight = CalendarLib.Time.midnight () in
  let n1 = D.days_in_month month in
  let n0 = D.days_in_month start in
  let d0 = D.day_of_month start - n0 in
  let prev_button =
    let a = [a_class ["fa"; "fa-angle-left"]] in
    Html5.D.span ~a [pcdata "previous"]
  and next_button =
    let a = [a_class ["fa"; "fa-angle-right"]] in
    Html5.D.span ~a [pcdata "next"] in
  let thead =
    thead
      [tr [th ~a:[a_colspan 7; a_class ["header"]]
             [pcdata (CalendarLib.Printer.Date.sprint "%B %Y" month);
              prev_button; next_button]];
       tr (List.map (fun d -> th [pcdata d]) days)]
  and f_cell i j =
    let module C = CalendarLib.Calendar in
    let d = d0 + j + 7 * i in
    let out = if d <= 0 || d > n1 then ["out"] else [] in
    let startdate =
      C.create (D.add start (D.Period.day (j + 7 * i))) midnight
    in
    let enddate = C.next startdate `Day in
    let sel =
      if
        let f (s, e, _) =
          C.compare
            (to_utc s) enddate < 0 &&
          C.compare
            startdate (to_utc e) < 0 in
        List.exists f events
      then
        ["sel"]
      else
        []
    in
    let sel =
      if
        C.compare startdate now <= 0 && C.compare now enddate < 0
      then
        "today" :: sel
      else
        sel
    in
    let d = if d > n1 then d - n1 else d in
    let d = if d <= 0 then d + n0 else d in
    ([a_class (out @ sel)], [div [pcdata @@ string_of_int @@ d]])
  and f_a_row i = [] in
  (build_table 5 6 ~a:[a_class ["calendar"]] ~thead ~f_cell ~f_a_row,
   prev_button,
   next_button)

}} ;;

{client{

    let attach_events day cal (events : 'a event list) =
      let module D = CalendarLib.Date in
      let month =
        D.nth_weekday_of_month (D.year day) (D.month day) D.Sun 1 in
      let start = D.prev month `Week in
      let midnight = CalendarLib.Time.midnight () in
      let rows = (Html5.To_dom.of_table cal)##rows in
      let f i =
        let (>>!) = Js.Opt.iter in
        rows##item (i + 2) >>! fun r ->
        let cells = r##cells in
        let f j =
          cells##item (j) >>! fun c ->
          let startdate =
            CalendarLib.Calendar.create
              (D.add start (D.Period.day (j + 7 * i))) midnight in
          let enddate =
            CalendarLib.Calendar.next startdate `Day in
          let l =
            List.filter
              (fun (s, e, _) ->
                 CalendarLib.Calendar.compare
                   (to_utc s) enddate < 0 &&
                 CalendarLib.Calendar.compare
                   startdate (to_utc e) < 0)
              events
          in
          match l with
          | [] -> ()
          | _ :: _ ->
            c##classList##add (Js.string "sel");
            c##onclick <- Dom_html.handler (fun _ -> Js._false)
        in
        iter_interval 0 6 f
      in
      iter_interval 0 5 f

let rec attach_behavior ?events ?get_events day (cal, prev, next) =
  let update d =
    let (cal', _, _) as c = build_calendar ?events d in
    attach_behavior ?events ?get_events d c;
    let cal = Html5.To_dom.of_element cal in
    let cal' = Html5.To_dom.of_element cal' in
    Js.Opt.iter (cal##parentNode)
      (fun parent -> Dom.replaceChild parent cal' cal);
    Js._false
  in
  let module D = CalendarLib.Date in
  let day =
    D.lmake ~year:(D.year day) ~month:(D.int_of_month (D.month day)) ()
  in
  (match events, get_events with
   | Some events, _ ->
     attach_events day cal events
   | None, Some get_events ->
     Lwt.async
       (fun () ->
          lwt events = get_events day in attach_events day cal events;
          Lwt.return ())
   | None, None ->
     ());
  (Html5.To_dom.of_element prev)##onclick <-
    Dom_html.handler (fun _ -> update (D.prev day `Month));
  (Html5.To_dom.of_element next)##onclick <-
    Dom_html.handler (fun _ -> update (D.next day `Month))

}}

{shared{

    let make ?events ?get_events () =
      let today = CalendarLib.Date.today () in
      let (cal, _, _) as c =
        build_calendar ?events today in
      ignore
        {unit{
            attach_behavior
              ?events:%events ?get_events:%get_events %today %c }};
      cal

  }}
