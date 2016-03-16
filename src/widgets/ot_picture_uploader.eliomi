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

(** [Ot_picture_uploader] allow the user to see a picture he wants to
    send to the before sending it server.
    Also, controllers can be added to allow the user to specify a cropping
    area. No cropping is actually done on the client side, it MUST be handled
    on server side by a service. *)

[%%shared.start]
type 'a service =
  (unit
  , 'a * ( (float * float * float * float) Eliom_lib.Option.t
           * Eliom_lib.file_info )
  , Eliom_service.post_service_kind
  , Eliom_service.non_attached_kind
  , [ `NonattachedCoservice ]
  , [ `WithoutSuffix ]
  , unit
  , [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name
    * ([ `One of (float * float * float * float)
             Eliom_lib.Option.t Eliom_parameter.ocaml ]
         Eliom_parameter.param_name
       * [ `One of Eliom_lib.file_info ] Eliom_parameter.param_name)
  , [ `Registrable ]
  , unit Eliom_service.ocaml_service) Eliom_service.service

[%%client.start]

(** [ let (reset, cropping, cropper_dom) = cropper ~image () ]
    [ reset ] is function to call to reset the current cropper status
    [ cropping ] are current cropping parameters
    [ cropper_dom ] is the div containing cropping controllers *)
val cropper :
  image:Dom_html.element Js.t Eliom_client_common.client_value
  -> ?ratio:float
  -> unit
  -> (unit -> unit)
     * (float * float * float * float) Eliom_shared.React.S.t
     * [> Html5_types.div ] Eliom_content.Html5.D.elt

(** [bind_input input preview ?container ?reset ()]
    Bind [input] and [preview] so the file selected in [input] is the
    image displayed in [preview].
    [?container] is used to toggle [ot-no-file] class.
    [?reset] is called when the [input] value changes. *)
val bind_input :
  Dom_html.inputElement Js.t Eliom_client_common.client_value
  -> Dom_html.imageElement Js.t Eliom_client_common.client_value
  -> ?container:Dom_html.element Js.t Eliom_client_common.client_value
  -> ?reset:(unit -> unit)
  -> unit
  -> unit

(** [ do_submit input ?cropping ~service ~arg () ]
    [input] is the input with file loaded
    [cropping] are cropping info
    [service] service used for submission
    [arg] extra argument passed to [service] (as first argument) *)
val do_submit :
  Dom_html.inputElement Js.t Eliom_client_common.client_value
  -> ?cropping:(float * float * float * float) Eliom_shared.React.S.t
  -> service:'a service
  -> arg:'a
  -> unit
  -> unit Lwt.t

(** [ bind_submit input button ?cropping ~service ~arg ~close () ]
    Make [ button ] action is to call [service] with
    [ (arg, (cropping, file)) ] parameters, [ file ] being the file select in
    [ input ].
    [ close ] is called once [ service ] returned. *)
val bind_submit :
  Dom_html.inputElement Js.t Eliom_client_common.client_value
  -> #Dom_html.eventTarget Js.t Eliom_client_common.client_value
  -> ?cropping:(float * float * float * float) Eliom_shared.React.S.t
  -> service:'a service
  -> arg:'a
  -> close:(unit -> unit Lwt.t)
  -> unit
  -> unit

(** [bind] is a shortcut for [bind_input] and [bind_submit] actions *)
val bind :
  ?container:Dom_html.element Js.t Eliom_client_common.client_value
  ->input:Dom_html.inputElement Js.t Eliom_client_common.client_value
  -> preview:#Dom_html.imageElement Js.t
  -> ?crop:( (unit -> unit)
             * (float * float * float * float) Eliom_shared.React.S.t )
  -> submit:#Dom_html.eventTarget Js.t Eliom_client_common.client_value
  -> service:'b service
  -> arg:'b
  -> close:(unit -> unit Lwt.t)
  -> unit
  -> unit

[%%shared.start]

(** Create a file input element with good input type [ot-pup-input] class
    and wrap it into a label.
    Return (input node, label node) *)
val input :
  [< Html5_types.label_content_fun > `Input ] Eliom_content.Html5.F.elt list
  -> ([> Html5_types.input ] Eliom_content.Html5.D.elt
      * [> Html5_types.label ] Eliom_content.Html5.D.elt)

(** Create a img element with no src, no alt and [ot-pup-preview] class. *)
val preview : unit -> [> Html5_types.img ] Eliom_content.Html5.D.elt

(** Create a button with [ot-pup-sumit] clas *)
val submit :
  [< Html5_types.button_content ] Eliom_content.Html5.F.elt list
  -> [> Html5_types.button ] Eliom_content.Html5.D.elt

(** [mk_service name arg_deriver]
    Create a named service taking [(arg_deriver, (cropping, file))] parameter *)
val mk_service : string -> 'a Deriving_Json.t -> 'a service

(** Ready-to-use form, using [service] and [arg]. Customizable with
    [input], the input button content, [submit], the submit button content.
    If [crop] is present, cropping is enable, with the optional ratio it is. *)
val mk_form :
  ?close:(unit -> unit Lwt.t)
  -> ?crop:float option
  -> ?input:[< Html5_types.label_content_fun > `Input ]
    Eliom_content.Html5.F.elt list
  -> ?submit:[< Html5_types.button_content_fun ]
    Eliom_content.Html5.D.Raw.elt
    Eliom_content.Html5.D.Raw.list_wrap
  -> 'a service
  -> 'a
  -> [> Html5_types.form ] Eliom_content.Html5.D.elt Lwt.t
