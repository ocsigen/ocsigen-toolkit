(** This module defines an interface to create icons with predefined
 * style/value. You need the CSS file "ot_icons.css" which defines the ot-icon
 * and other used CSS classes for predefined icons.
 *)

[%%shared
 module Make(A : sig
              module Raw : Eliom_content_html_raw.RAW
              include module type of Raw
              include Eliom_content_sigs.LINKS_AND_FORMS
                      with type +'a elt := 'a elt
                       and type +'a attrib := 'a attrib
                       and type uri := uri
                       and type ('a, 'b, 'c) star := ('a, 'b, 'c) star
                       and type 'a form_param :=
                             'a Eliom_content.Html.form_param
            end) = struct

  (** [icon classes] create an icon HTML attribute with "ot-icon" and [classes]
   * as CSS classes.
   * The optional parameter is at the end to be able to add other CSS classes
   * with predefined icons.
   *)
  let icon classes
      ?(a = ([] : Html_types.i_attrib Eliom_content.Html.attrib list)) () =
    A.i ~a:(A.a_class ("ot-icon" :: classes) :: a) []

  (* Predefined icons. See ot-icons.css *)
  let user      = icon [ "ot-icon-user" ]
  let plus      = icon [ "ot-plus" ]
  let spinner   = icon [ "ot-icon-spinner"; "ot-icon-animation-spinning" ]
  let shutdown  = icon [ "ot-icon-power" ]
  let config    = icon [ "ot-icon-gear" ]
  let signout   = icon [ "ot-icon-sign-out" ]
  let close     = icon [ "ot-icon-close" ]
  let question  = icon [ "ot-icon-question" ]

end

module F = Make(Eliom_content.Html.F)
module D = Make(Eliom_content.Html.D)

]
