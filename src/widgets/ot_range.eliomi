{shared{

type irp = int React.signal * (?step:React.step -> int -> unit)

val make :
  ?txt_up : string ->
  ?txt_down : string ->
  ?f : (int -> string) ->
  ?lb : int ->
  int ->
  [> Html5_types.div ] Eliom_content.Html5.F.elt

}}
