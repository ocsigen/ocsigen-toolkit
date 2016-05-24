[%%shared
module Make(A : module type of Eliom_content.Html.F) = struct

  let icon classes
      ?(a = ([] : Html_types.i_attrib Eliom_content.Html.attrib list)) () =
    A.i ~a:(A.a_class ("ot-icon" :: classes) :: a) []

  let user = icon [ "ot-icon-user" ]
  let plus = icon [ "ot-plus" ]
  (* let envelope = icon ["fa-envelope"; "fa-fw" ] *)
  (* let logout = icon [ "ot-logout" ] *)
  let spinner = icon [ "ot-icon-spinner"; "ot-icon-animation-spinning" ]
  (* let file = icon ["fa-file"; "fa-fw"] *)
  (* let download = icon ["fa-cloud-download"; "fa-fw"] *)
  (* let share = icon ["fa-share"; "fa-fw"] *)
  let shutdown = icon [ "ot-icon-power" ]
  let config = icon [ "ot-icon-gear" ]
  let signout = icon [ "ot-icon-sign-out" ]
  let close = icon [ "ot-icon-close" ]
  let question = icon [ "ot-icon-question" ]

end

module F = Make(Eliom_content.Html.F)
module D = Make(Eliom_content.Html.D)

]
