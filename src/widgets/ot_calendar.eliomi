{shared{

    (** For a handler [(get, act)], [get m y] provides the dates that
        need to be handled for month [m] of year [y]. [act d] is the
        client-side action to be performed for the day [d] (that needs
        to be in [get m y]). *)

type handler =
  ((int -> int -> int list Lwt.t) Eliom_lib.client_value *
   (int -> unit) Eliom_lib.client_value)

val make :
  ?handler:handler -> unit ->
  [> Html5_types.table ] Eliom_content.Html5.elt

}}
