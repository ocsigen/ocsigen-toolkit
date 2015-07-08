{shared{

    (** [make_hours_minutes f] produces a clock-style time picker for
        hours and minutes. When the user picks the time [h]:[m] , we
        call [f (h, m)] on the client side. *)

val make_hours_minutes :
  (int * int -> unit) Eliom_lib.client_value ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt

(** [make_hours f] produces a clock-style hour picker. When the user
    picks an hour [h], we call [f h] on the client side. *)

val make_hours :
  (int -> unit) Eliom_lib.client_value ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt

(** [make_minutes f] produces a clock-style minute picker. When the
    user picks an hour [m], we call [f m] on the client side. *)

val make_minutes :
  (int -> unit) Eliom_lib.client_value ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt

val make_hours_minutes_seq :
  unit -> Html5_types.div Eliom_content.Html5.elt

}}
