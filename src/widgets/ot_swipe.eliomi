[%%shared.start]

(**
   [bind ~compute_final_pos elt] makes [elt] swipable on touch screens.
   [compute_final_pos] is a function that will compute the final position
   of the element w.r.t. the position where it has been released (in pixels).

   Use [?min] and [?max] if you want to limit the displacement.
 *)
val bind:
  ?transition_duration : float ->
  ?min : int ->
  ?max : int ->
  compute_final_pos: (int -> int) Eliom_client_value.t ->
  'a Eliom_content.Html.elt ->
  unit

(**/**)
[%%client.start]
val clX : Dom_html.touchEvent Js.t -> int
val clY : Dom_html.touchEvent Js.t -> int
