[%%shared.start]

(**
   [bind ~compute_final_pos elt] makes [elt] swipable on touch screens.
   [compute_final_pos] is a function that will compute the final position
   of the element w.r.t. the position where it has been released (in pixels).
 *)
val bind:
  ?transition_duration : float ->
  compute_final_pos: (int -> int) Eliom_client_value.t ->
  'a Eliom_content.Html.elt ->
  unit
