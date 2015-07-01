{shared{

    (** [time_picker ~discrete x f] produces a clock-style time picker
        whose size is [x] * [x] pixels. When the user picks the time
        [h]:[m] , we call [f h m] on the client side. If [discrete] is
        [true], the user can only click on the twelve hours, i.e., m
        will always be zero. *)

val time_picker :
  ?discrete:bool ->
  (int -> int -> unit) Eliom_lib.client_value ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt

}}
