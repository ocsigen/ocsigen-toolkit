{shared{

val make :
  ?txt_up : string ->
  ?txt_down : string ->
  ?lb : int ->
  ub : int ->
  (int -> string) ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  int React.signal Eliom_pervasives.client_value

}}
