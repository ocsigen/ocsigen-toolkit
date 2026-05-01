(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2025
 *      Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Helpers to build [<i>] icon elements styled by the toolkit's CSS.

    Each icon is rendered as an empty [<i class="ot-icon ...">] element
    whose CSS classes select the actual glyph. The toolkit ships an
    [ot_icons.css] stylesheet that defines all the [ot-icon-*] and
    [ot-*] classes used by the predefined icons below. Include that
    file (or your own equivalent) in your application for the icons to
    actually render.

    Use {!Ot.Icons.F} for static HTML and {!Ot.Icons.D} for HTML with
    DOM identity (typically when you need to refer to the produced
    element from client code), the same way Eliom exposes
    {!Eliom.Content.Html.F} and {!Eliom.Content.Html.D}. *)

[%%shared.start]

(** Output of {!Make}: a primitive [icon] builder plus a handful of
    predefined icons for common UI actions. *)
module type S = sig
  type 'a elt
  type 'a attrib

  (** [icon classes ()] is an empty [<i>] element whose CSS classes
      are ["ot-icon"] followed by [classes]. The optional [?a]
      argument lets the caller add extra attributes (typically more
      classes). It comes last so that the predefined icons below can
      be partially applied and still expose [?a]. *)
  val icon :
     string list
    -> ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Predefined icon for a user / profile glyph
      (CSS class [ot-icon-user]). *)
  val user :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Predefined "plus" / add icon (CSS class [ot-plus]). *)
  val plus :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Animated spinner icon
      (CSS classes [ot-icon-spinner ot-icon-animation-spinning]). *)
  val spinner :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Shutdown / power icon (CSS class [ot-icon-power]). *)
  val shutdown :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Configuration / gear icon (CSS class [ot-icon-gear]). *)
  val config :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Sign out / logout icon (CSS class [ot-icon-sign-out]). *)
  val signout :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Close icon (CSS class [ot-icon-close]). *)
  val close :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt

  (** Question mark / help icon (CSS class [ot-icon-question]). *)
  val question :
     ?a:Html_types.i_attrib attrib list
    -> unit
    -> [> Html_types.i] elt
end

(** Build an icon module on top of an Eliom HTML implementation
    ({!Eliom.Content.Html.F} or {!Eliom.Content.Html.D}). *)
module Make (A : Eliom.Content.Html.T) :
  S with type 'a elt = 'a A.elt and type 'a attrib = 'a A.attrib

(** Icons built with {!Eliom.Content.Html.F} (static HTML). *)
module F : S
  with type 'a elt = 'a Eliom.Content.Html.F.elt
   and type 'a attrib = 'a Eliom.Content.Html.F.attrib

(** Icons built with {!Eliom.Content.Html.D} (HTML with DOM identity,
    suitable for being referenced from client-side code). *)
module D : S
  with type 'a elt = 'a Eliom.Content.Html.D.elt
   and type 'a attrib = 'a Eliom.Content.Html.D.attrib
