{client{

    (** [time_picker ~discrete x f] produces a clock-style time picker
        whose size is [x] * [x] pixels. When the user picks the time
        [h]:[m] , we call [f h m]. If [discrete] is [true], the user
        can only click on the twelve hours, i.e., m will always be
        zero. *)

val time_picker :
  ?discrete:bool ->
  int -> (int -> int -> unit) ->
  [> Html5_types.svg ] Eliom_content.Html5.F.elt

}}
