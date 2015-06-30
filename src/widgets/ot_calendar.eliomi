{shared{

type 'a event =
  CalendarLib.Calendar.t * CalendarLib.Calendar.t * 'a

val calendar :
  ?events:'a event list ->
  ?get_events:(CalendarLib.Date.t -> 'a event list Lwt.t) ->
  unit ->
  [> Html5_types.table ] Eliom_content.Html5.D.elt

}}
