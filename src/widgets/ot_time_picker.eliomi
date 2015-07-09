{shared{

    (** [make_hours_minutes ()] produces a clock-style time picker for
        hours and minutes. The output is a reactive signal [(h, m)]
        where [h] are the hours and [m] the minutes that the user
        picked. *)

val make_hours_minutes :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  (int * int) React.signal Eliom_pervasives.client_value

(** [make_hours_minutes ()] produces a clock-style time picker for
    hours and minutes. The user is first asked to pick hours, then
    minutes with a separate clock. The output is a reactive signal
    [(h, m)] where [h] are the hours and [m] the minutes that the user
    picked. *)

val make_hours_minutes_seq :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  (int * int) React.signal Eliom_pervasives.client_value

(** [make_hours f] produces a clock-style hour picker. *)

val make_hours :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  int React.signal Eliom_pervasives.client_value

val make_hours_24h :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  int React.signal Eliom_pervasives.client_value

(** [make_minutes f] produces a clock-style minute picker. *)

val make_minutes :
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  int React.signal Eliom_pervasives.client_value

}}
