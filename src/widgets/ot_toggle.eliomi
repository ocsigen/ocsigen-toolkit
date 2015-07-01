{server{

val display :
  ?init_up:bool ->
  ?up_txt:string ->
  ?down_txt:string ->
  unit ->
  [> Html5_types.div ]
    Eliom_content.Html5.F.elt
    Eliom_csreact.SharedReact.S.t *
  bool Eliom_csreact.SharedReact.S.t

}}
