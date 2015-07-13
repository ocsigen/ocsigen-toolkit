{shared{

    (** Given a handler [(get, act)], [get y m] provides the dates
        that need to be handled for month [m] of year [y]. [act y m d]
        is the client-side action to be performed for the day
        [y]:[m]:[d] (where [d] needs to be in [get y m]). *)

type handler =
  ((int -> int -> int list Lwt.t) Eliom_lib.client_value *
   (int -> int -> int -> unit) Eliom_lib.client_value)

val make :
  ?handler:handler -> unit ->
  [> Html5_types.table ] Eliom_content.Html5.elt

}}
