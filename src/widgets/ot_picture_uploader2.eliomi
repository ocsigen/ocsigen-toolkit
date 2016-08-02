(* Ocsigen
 *  http://www.ocsigen.org
 *
 * Copyright (C) 2015 BeSport, Julien Sagot
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

(** Picture uploader widget *)

(** [Ot_picture_uploader] allow the user to see a picture he wants to
    send to the before sending it server.  Also, controllers can be
    added to allow the user to specify a cropping area. No cropping is
    actually done on the client side, it MUST be handled on server
    side by a service. *)

[%%shared.start]
type cropping = (float * float * float * float) React.S.t

[%%client.start]

(** [ let (reset, cropping, cropper_dom) = cropper ~image () ]
    [ reset ] is function to call to reset the current cropper status
    [ cropping ] are current cropping parameters
    [ cropper_dom ] is the div containing cropping controllers *)
val cropper :
  image:Dom_html.element Js.t Eliom_client_value.t
  -> ?ratio:float
  -> unit
  -> (unit -> unit)
     * cropping
     * [> `Div ] Eliom_content.Html.elt

(** [bind_input input preview ?container ?reset ()]
    Bind [input] and [preview] so the file selected in [input] is the
    image displayed in [preview].
    [?container] is used to toggle [ot-no-file] class.
    [?reset] is called when the [input] value changes. *)
val bind_input :
  Dom_html.inputElement Js.t Eliom_client_value.t
  -> Dom_html.imageElement Js.t Eliom_client_value.t
  -> ?container:#Dom_html.element Js.t Eliom_client_value.t
  -> ?reset:(unit -> unit)
  -> unit
  -> unit

(** [ do_submit input ?cropping ~service ~arg () ]
    [input] is the input with file loaded
    [cropping] are cropping info
    [service] service used for submission
    [arg] extra argument passed to [service] (as first argument) *)
val do_submit :
  Dom_html.inputElement Js.t Eliom_client_value.t
  -> ?cropping:cropping
  -> upload:(?cropping:cropping -> File.file Js.t -> unit Lwt.t)
  -> unit
  -> unit Lwt.t

(** [ bind_submit input button ?cropping ~service ~arg ~after_submit () ]
    Make [ button ] action is to call [service] with
    [ (arg, (cropping, file)) ] parameters, [ file ] being the file select in
    [ input ].
    [ after_submit ] is called once [ service ] returned. *)
val bind_submit :
  Dom_html.inputElement Js.t Eliom_client_value.t
  -> #Dom_html.eventTarget Js.t Eliom_client_value.t
  -> ?cropping:cropping
  -> upload:(?cropping:cropping -> File.file Js.t -> unit Lwt.t)
  -> after_submit:(unit -> unit Lwt.t)
  -> unit
  -> unit

(** [bind] is a shortcut for [bind_input] and [bind_submit] actions *)
val bind :
  ?container:#Dom_html.element Js.t Eliom_client_value.t
  -> input:Dom_html.inputElement Js.t Eliom_client_value.t
  -> preview:Dom_html.imageElement Js.t
  -> ?crop:( (unit -> unit) * cropping )
  -> submit:#Dom_html.eventTarget Js.t Eliom_client_value.t
  -> upload:(?cropping:cropping -> File.file Js.t -> unit Lwt.t)
  -> after_submit:(unit -> unit Lwt.t)
  -> unit
  -> unit

[%%shared.start]

(** Create a file input element with good input type [ot-pup-input] class
    and wrap it into a label.
    Return (input node, label node) *)
val input :
  ?a:[< Html_types.label_attrib > `Class ] Eliom_content.Html.attrib list
  -> [< Html_types.label_content_fun ] Eliom_content.Html.elt list
  -> ([> `Input ] Eliom_content.Html.elt
      * [> `Label ] Eliom_content.Html.elt)

(** Create a img element with no src, no alt and [ot-pup-preview] class. *)
val preview :
  ?a:[< Html_types.img_attrib > `Class ] Eliom_content.Html.attrib list
  -> unit -> [> `Img ] Eliom_content.Html.elt

(** Create a button with [ot-pup-sumit] clas *)
val submit :
  ?a:[< Html_types.button_attrib > `Class ] Eliom_content.Html.attrib list
  -> [< Html_types.button_content ] Eliom_content.Html.elt list
  -> [> `Button ] Eliom_content.Html.elt

(** Ready-to-use form, using [service] and [arg]. Customizable with
    [input], the input button content, [submit], the submit button content.
    If [crop] is present, cropping is enable, with the optional ratio it is. *)
val mk_form :
  ?after_submit:(unit -> unit Lwt.t)
  -> ?crop:float option
  -> ?input:([< Html_types.label_attrib > `Class ] Eliom_content.Html.attrib list
             * [< Html_types.label_content_fun ] Eliom_content.Html.elt list)
  -> ?submit:([< Html_types.button_attrib > `Class ] Eliom_content.Html.attrib list
              * [< Html_types.button_content_fun ] Eliom_content.Html.elt list)
  -> (?cropping:cropping -> File.file Js.t -> unit Lwt.t)
  -> [> `Form ] Eliom_content.Html.elt Lwt.t
