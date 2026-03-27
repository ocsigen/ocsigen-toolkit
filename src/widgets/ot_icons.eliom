(** This module defines an interface to create icons with predefined
 * style/value. You need the CSS file "ot_icons.css" which defines the ot-icon
 * and other used CSS classes for predefined icons.
 *)

[%%shared
module type ICON_HTML = sig
  val i :
     ?a:[< Html_types.i_attrib] Eliom.Content.Html.attrib list
    -> [< Html_types.i_content_fun] Eliom.Content.Html.elt list
    -> [> Html_types.i] Eliom.Content.Html.elt
  val a_class : Html_types.nmtokens -> [> `Class] Eliom.Content.Html.attrib
end

module Make (A : ICON_HTML) = struct
  (** [icon classes] create an icon HTML attribute with "ot-icon" and [classes]
   * as CSS classes.
   * The optional parameter is at the end to be able to add other CSS classes
   * with predefined icons.
   *)
  let icon
        classes
        ?(a = ([] : Html_types.i_attrib Eliom.Content.Html.attrib list))
        ()
    =
    A.i ~a:(A.a_class ("ot-icon" :: classes) :: a) []

  (* Predefined icons. See ot-icons.css *)
  let user = icon ["ot-icon-user"]
  let plus = icon ["ot-plus"]
  let spinner = icon ["ot-icon-spinner"; "ot-icon-animation-spinning"]
  let shutdown = icon ["ot-icon-power"]
  let config = icon ["ot-icon-gear"]
  let signout = icon ["ot-icon-sign-out"]
  let close = icon ["ot-icon-close"]
  let question = icon ["ot-icon-question"]
end

module F = Make (Eliom.Content.Html.F)
module D = Make (Eliom.Content.Html.D)]
