[%%shared.start]

(**
   [bind ~compute_final_pos elt] makes [elt] swipable on touch screens.
   [compute_final_pos] is a function that will compute the final position
   of the element w.r.t. the position where it has been released (in pixels).

   Use [?min] and [?max] if you want to limit the displacement.
   If outside range, events will be propagated to parent
   (which makes it possible to have a swipeable element inside another one).

   [?onstart] and [?onend] can be used to execute some side effect when swipe
   starts or ends.
 *)
val bind:
  ?transition_duration : float ->
  ?min : int ->
  ?max : int ->
  compute_final_pos: (int -> int) Eliom_client_value.t ->
  ?onstart: (unit -> unit) Eliom_client_value.t ->
  ?onend: (unit -> unit) Eliom_client_value.t ->
  'a Eliom_content.Html.elt ->
  unit

(**/**)
[%%client.start]
val clX : Dom_html.touchEvent Js.t -> int
val clY : Dom_html.touchEvent Js.t -> int
val threshold : int
