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

[%%shared.start]

open Js_of_ocaml

(** {2 Picture uploader widget}

    [Ot_picture_uploader] allows the user to see a picture he wants to
    send to the before sending it server.  Also, controllers can be
    added to allow the user to specify a cropping area. No cropping is
    actually done on the client side, it MUST be handled on server
    side. *)

type cropping = (float * float * float * float) React.S.t

type 'a upload =
  ?progress:(int -> int -> unit)
  -> ?cropping:cropping
  -> File.file Js.t
  -> 'a Lwt.t

type ('a, 'b) service =
  ( unit
  , 'a * ((float * float * float * float) option * Eliom_lib.file_info)
  , Eliom_service.post
  , Eliom_service.non_att
  , Eliom_service.co
  , Eliom_service.non_ext
  , Eliom_service.reg
  , [`WithoutSuffix]
  , unit
  , [`One of 'a Eliom_parameter.ocaml] Eliom_parameter.param_name
    * ([`One of (float * float * float * float) option Eliom_parameter.ocaml]
       Eliom_parameter.param_name
      * [`One of Eliom_lib.file_info] Eliom_parameter.param_name)
  , 'b Eliom_service.ocaml )
  Eliom_service.t
(** a service that implements a function with type ['a -> 'b] *)

[%%client.start]

val ocaml_service_upload : service:('a, 'b) service -> arg:'a -> 'b upload

val cropper
  :  image:Dom_html.element Js.t Eliom_client_value.t
  -> ?ratio:float
  -> unit
  -> (unit -> unit) * cropping * [> `Div] Eliom_content.Html.elt
(** [ let (reset, cropping, cropper_dom) = cropper ~image () ]
    [ reset ] is function to call to reset the current cropper status
    [ cropping ] are current cropping parameters
    [ cropper_dom ] is the div containing cropping controllers *)

val bind_input
  :  Dom_html.inputElement Js.t Eliom_client_value.t
  -> Dom_html.imageElement Js.t Eliom_client_value.t
  -> ?container:#Dom_html.element Js.t Eliom_client_value.t
  -> ?reset:(unit -> unit)
  -> unit
  -> unit
(** [bind_input input preview ?container ?reset ()]
    Bind [input] and [preview] so the file selected in [input] is the
    image displayed in [preview].
    [?container] is used to toggle [ot-no-file] class.
    [?reset] is called when the [input] value changes. *)

val do_submit
  :  Dom_html.inputElement Js.t Eliom_client_value.t
  -> ?progress:(int -> int -> unit)
  -> ?cropping:cropping
  -> upload:'a upload
  -> unit
  -> unit Lwt.t
(** [ do_submit input ?cropping ~upload () ]
    [input] is the input with file loaded
    [cropping] are cropping info
    [upload] function to upload the file *)

val bind_submit
  :  Dom_html.inputElement Js.t Eliom_client_value.t
  -> #Dom_html.eventTarget Js.t Eliom_client_value.t
  -> ?cropping:cropping
  -> upload:'a upload
  -> after_submit:(unit -> unit Lwt.t)
  -> unit
  -> unit
(** [ bind_submit input button ?cropping ~upload ~after_submit () ]
    binds the following two actions to [ button ] when it is being clicked:
    call [ do_submit ] which uploads the file; then call [ after_submit ] *)

val bind
  :  ?container:#Dom_html.element Js.t Eliom_client_value.t
  -> input:Dom_html.inputElement Js.t Eliom_client_value.t
  -> preview:Dom_html.imageElement Js.t
  -> ?crop:(unit -> unit) * cropping
  -> submit:#Dom_html.eventTarget Js.t Eliom_client_value.t
  -> upload:'a upload
  -> after_submit:(unit -> unit Lwt.t)
  -> unit
  -> unit
(** [bind] is a shortcut for [bind_input] and [bind_submit] actions *)

[%%shared.start]

val input
  :  ?a:[< Html_types.label_attrib > `Class] Eliom_content.Html.attrib list
  -> [< Html_types.label_content_fun] Eliom_content.Html.elt list
  -> [> `Input] Eliom_content.Html.elt * [> `Label] Eliom_content.Html.elt
(** Create a file input element with good input type [ot-pup-input] class
    and wrap it into a label.
    Return (input node, label node) *)

val preview
  :  ?a:[< Html_types.img_attrib > `Class] Eliom_content.Html.attrib list
  -> unit
  -> [> `Img] Eliom_content.Html.elt
(** Create a img element with no src, no alt and [ot-pup-preview] class. *)

val submit
  :  ?a:[< Html_types.button_attrib > `Class] Eliom_content.Html.attrib list
  -> [< Html_types.button_content] Eliom_content.Html.elt list
  -> [> `Button] Eliom_content.Html.elt
(** Create a button with [ot-pup-sumit] clas *)

val mk_form
  :  ?after_submit:(unit -> unit Lwt.t)
  -> ?crop:float option
  -> ?input:
       [< Html_types.label_attrib > `Class] Eliom_content.Html.attrib list
       * [< Html_types.label_content_fun] Eliom_content.Html.elt list
  -> ?submit:
       [< Html_types.button_attrib > `Class] Eliom_content.Html.attrib list
       * [< Html_types.button_content_fun] Eliom_content.Html.elt list
  -> 'a upload
  -> [> `Form] Eliom_content.Html.elt Lwt.t
(** Ready-to-use form. Customizable with
    [input], the input button content, [submit], the submit button content.
    If [crop] is present, cropping is enable, with the optional ratio it is.
    The last argument determines the method by which the file is uploaded.
    *)

[%%server.start]

val mk_service : string -> 'a Deriving_Json.t -> ('a, 'b) service
(** [mk_service name arg_deriver] Create a named service taking
    [(arg_deriver, (cropping, file))] parameter *)
