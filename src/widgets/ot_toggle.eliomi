{shared{

val make :
  ?init_up:bool ->
  ?up_txt:string ->
  ?down_txt:string ->
  unit ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt *
  bool React.signal Eliom_pervasives.client_value

}}
