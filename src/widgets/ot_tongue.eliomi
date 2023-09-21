[%%shared.start]

type simple_stop = [`Percent of int | `Px of int | `Full_content]

type stop =
  [ `Percent of int
  | `Px of int
  | `Full_content
  | `Interval of simple_stop * simple_stop ]

type tongue =
  { elt : Html_types.div Eliom_content.Html.D.elt
  ; stop_signal_before : simple_stop React.S.t Eliom_client_value.t
  ; stop_signal_after : simple_stop React.S.t Eliom_client_value.t
  ; swipe_pos : int React.S.t Eliom_client_value.t
  ; px_signal_before : int React.S.t Eliom_client_value.t
  ; px_signal_after : int React.S.t Eliom_client_value.t }
(** Signals contain the current
    position of the tongue, as a [simple_stop] or as [int].
    Before (resp. after) signals are triggered before (resp. after) transition.
    [swipe_pos] represents the position during the swipe of the user *)

val tongue :
   ?a:[< Html_types.div_attrib] Eliom_content.Html.attrib list
  -> ?side:[`Bottom | `Left | `Right | `Top]
  -> ?stops:stop list
  -> ?init:simple_stop
  -> ?handle:[> `Div] Eliom_content.Html.elt
  -> ?update:simple_stop React.event Eliom_client_value.t
  -> [< Html_types.div_content_fun] Eliom_content.Html.elt list
  -> tongue
(**
   A tongue is an expandable panel that can appear from a side of the screen
   (usually top or bottom). It is usually intended for touchscreen interfaces.
   It takes its content as main parameter.

   This function takes the content of the tongue and returns a value of type
   [tongue].

   [?side] is the side of the screen where the tongue is attached.
   By default [`Bottom].

   [?stops] is the list of positions you want the tongue to stick to.
   By default,
   it is [[`Px 70; `Percent 100; `Interval (`Percent 100, `Full_content)]],
   which means: only 70 pixels visible,
   screen size, and any value between screen size
   and content size.

   The percentage is given w.r.t. windows height or width.
   Size of the tongue will never exceed content size.

   [?init] is the initial position of the tongue. Usually element of [~stops].
   By default [`Px 70].

   [?handle] is the element that will we used as handle to move the tongue
   (usually an element of the content).
   It must be a D element.
   By default, it is the whole tongue.

   [?update] is a react event you can use to command the tongue position
   from outside.

*)
