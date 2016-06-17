[%%shared.start]

open Eliom_content.Html
open Eliom_content.Html.F


let%shared dropdown ?(a = []) ~menu content =
  D.div ~a:(a_class ["ot-dropdown"] :: a)
    (content @ [div ~a:[a_class ["ot-dropdown-menu"]] menu])
