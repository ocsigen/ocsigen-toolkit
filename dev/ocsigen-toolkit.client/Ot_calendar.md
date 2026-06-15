
# Module `Ot_calendar`

```ocaml
type intl = {
  i_days : string list;
  i_months : string list;
  i_start : [ `Sun | `Mon | `Tue | `Wed | `Thu | `Fri | `Sat ];
}
```
`intl` is the type of internationalization specifiers. `i_days` contains the names of the weekdays, starting with Sunday. `i_months` contains the names of the months, starting with January. `i_start` specifies the first day of the week.

```ocaml
type button_labels = {
  b_prev_year : string;
  b_prev_month : string;
  b_next_month : string;
  b_next_year : string;
}
```
An instance of `button_labels` is used to customize the button labels. The defaults are "\<\<", "\<", "\>", and "\>\>".

```ocaml
val make : 
  ?init:(int * int * int) ->
  ?highlight:(int -> int -> int list Lwt.t) Eliom_client_value.t ->
  ?click_non_highlighted:bool ->
  ?update:(int * int * int) React.E.t Eliom_client_value.t ->
  ?action:(int -> int -> int -> unit Lwt.t) Eliom_client_value.t ->
  ?period:
    (CalendarLib.Date.field CalendarLib.Date.date
     * CalendarLib.Date.field CalendarLib.Date.date) ->
  ?button_labels:button_labels ->
  ?intl:intl ->
  unit ->
  [> `Div ] Eliom_content.Html.elt
```
`make ?highlight ?click_any ?action` produces a calendar.

If a client-side function `highlight` is provided, `highlight y m` needs to produce the list of days for the month `m` of the year `y` that need to be visually denoted.

If `click_non_highlighted` is `true`, every date is clickable; otherwise, only the dates that `highlight` returns (if `highlight` is provided) are clickable.

If a client-side function `action` is provided, when the user clicks on the date `d`:`m`:`y`, `action y m d` is called.

If `period` is provided, the calendar will have a period restriction between the two dates given contained in `update`.

```ocaml
val make_date_picker : 
  ?init:(int * int * int) ->
  ?update:(int * int * int) React.E.t Eliom_client_value.t ->
  ?button_labels:button_labels ->
  ?intl:intl ->
  ?period:
    (CalendarLib.Date.field CalendarLib.Date.date
     * CalendarLib.Date.field CalendarLib.Date.date) ->
  unit ->
  [> `Div ] Eliom_content.Html.elt * (int * int * int) Eliom_shared.React.S.t
```
`make_date_picker ?init ()` returns a client-side reactive signal `(y, m, d)` corresponding to the date `d`:`m`:`y` that the user clicks on. The optional parameter `init` provides an initial value for the signal. `?intl` is used to internationalize the calendar (see [`intl`](./#type-intl)). The default behavior is for English.
