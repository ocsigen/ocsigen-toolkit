(* Ocsigen
 * http://www.ocsigen.org
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

TODO:
  REVOIR TOUT L'HISTORIQUE DE W_FORMS SUR BESPORT
    DOCUMENTER
    COMPLÉTER

(** Advanced forms.

    This module improves regular HTML forms elements to make them easier to use.
*)

[%%client.start]

open Js_of_ocaml
open Eliom_content.Html
open Html_types

(** An HTML element which can be selected by pressing the tab key *)
class type tabbable = object
  inherit Dom_html.element
  method tabIndex : int Js.prop
end

val setup_tabcycle : #tabbable Js.t list -> unit
(** [setup_tabcycle] makes tab key loop over child elements of an element and
    only these elements. *)

val setup_tabcycle_auto : Dom_html.element Js.t -> unit
(** [setup_tabcycle_auto] scans an element for tabbable elements (buttons, inputs)
    and feeds them to [setup_tabcycle] *)

val setup_form : Dom_html.element Js.t -> unit
(** Scan for focusable elements apply [setup_tabcycle_auto] to them and
    focus the first. *)

val prevent_tab : Dom_html.element Js.t -> unit -> unit
(** [prevent_tab e] prevents [e] (and its children) to be focused with tab key.
    A function to restore the initial status is returned. *)

[%%shared.start]

val reactive_input :
   ?a:[< Html_types.input_attrib] Eliom_content.Html.attrib list
  -> ?input_r:string Bs_lib.React.component
  -> ?value:string
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> unit
  -> [> `Input] Eliom_content.Html.elt
     * (string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t)
(** Input with reactive interface *)

val reactify_input :
   ?input_r:string Bs_lib.React.component
  -> ?value:string
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> [`Input | `Textarea] Eliom_content.Html.elt
  -> string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t
(** Transforms an existing input into a reactive input *)

val textarea :
   ?a:[< Html_types.textarea_attrib] Eliom_content.Html.attrib list
  -> ?a_rows:int
  -> ?resize:bool
  -> ?a_placeholder:string
  -> string
  -> [> `Textarea] Eliom_content.Html.elt
(** Textarea with the ability to resize itself automatically *)

val reactive_textarea :
   ?a:[< Html_types.textarea_attrib] Eliom_content.Html.attrib list
  -> ?a_rows:int
  -> ?resize:bool
  -> ?a_placeholder:string
  -> ?value:string
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> unit
  -> [> `Textarea] Eliom_content.Html.elt
     * (string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t)
(** Reactive interface for textarea *)

val lwt_bound_input_enter :
   ?a:[< Html_types.input_attrib] Eliom_content.Html.attrib list
  -> ?button:[`Button] Eliom_content.Html.elt
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> (string -> unit Lwt.t) Eliom_client_value.t
  -> [> `Input] Eliom_content.Html.elt

val lwt_bind_input_enter :
   ?validate:(string -> bool) Eliom_client_value.t
  -> ?button:[`Button] Eliom_content.Html.elt
  -> [`Input] Eliom_content.Html.elt
  -> (string -> unit Lwt.t) Eliom_client_value.t
  -> unit

val reactive_toggle_button :
   ?a:Html_types.button_attrib Eliom_content.Html.attrib list
  -> ?init:bool
  -> ?ctrl:
       bool Eliom_shared.React.S.t
       * (?step:React.step -> bool -> unit) Eliom_shared.Value.t
  -> check:(bool -> Bs_buttons.btn_icn * Bs_buttons.Emph.t)
  -> Html_types.span_content Eliom_content.Html.elt list
  -> [> `Button] Eliom_content.Html.elt
     * (bool Eliom_shared.React.S.t
       * (?step:React.step -> bool -> unit) Eliom_shared.Value.t)

val radio :
   ?a:[< Html_types.label_attrib] Eliom_content.Html.attrib list
  -> ?disabled_s:bool Eliom_shared.React.S.t
  -> ?checked_s:bool Eliom_shared.React.S.t
  -> ?name:string
  -> ?before:
       [< Html_types.label_content_fun > `Input `Span] Eliom_content.Html.elt
         list
  -> [< Html_types.span_content] Eliom_content.Html.elt list
  -> [> `Label] Eliom_content.Html.elt * [> `Input] Eliom_content.Html.elt

val radio_buttons :
   ?a:[< Html_types.label_attrib] Eliom_content.Html.attrib list
  -> ?disabled_s:int list Eliom_shared.React.S.t
  -> selection_react:int option Bs_lib.React.component
  -> name:string
  -> [< Html_types.span_content] Eliom_content.Html.elt list list
  -> Html_types.div_content Eliom_content.Html.elt list

val radio_selector :
   ?a:[< Html_types.div_attrib > `Class] Eliom_content.Html.attrib list
  -> selection_react:int option Bs_lib.React.component
  -> name:string
  -> label:[< Html_types.div_content] Eliom_content.Html.elt list
  -> [< Html_types.span_content] Eliom_content.Html.elt list list
  -> [> `Div] Eliom_content.Html.elt

type checkbox_position = [`Left | `Right | `None]
type checkbox_style = [`Box | `Bullet | `Small_bullet | `Toggle]

val checkbox :
   ?a:[< Html_types.label_attrib] Eliom_content.Html.attrib list
  -> ?a_inp:[< Html_types.input_attrib] Eliom_content.Html.attrib list
  -> ?required:bool
  -> ?position:checkbox_position
  -> ?checked:bool
  -> ?readonly:bool
  -> ?disabled:bool
  -> ?style:checkbox_style
  -> ?mobile_style:checkbox_style
  -> [< Html_types.span_content] Eliom_content.Html.elt list
  -> [> `Label] Eliom_content.Html.elt * [> `Input] Eliom_content.Html.elt

val reactive_checkbox :
   ?a:[< Html_types.label_attrib] Eliom_content.Html.attrib list
  -> ?a_inp:[< Html_types.input_attrib] Eliom_content.Html.attrib list
  -> ?position:checkbox_position
  -> ?checked:bool
  -> ?readonly:bool
  -> ?disabled:bool
  -> ?style:checkbox_style
  -> ?mobile_style:checkbox_style
  -> ?ctrl:
       bool Eliom_shared.React.S.t
       * (?step:React.step -> bool -> unit) Eliom_shared.Value.t
  -> [< Html_types.span_content] Eliom_content.Html.elt list
  -> < label : [> `Label] Eliom_content.Html.elt
     ; input : [> `Input] Eliom_content.Html.elt
     ; value : (* whether it is checked or not *)
         bool Eliom_shared.React.S.t
     ; manually_changed :
         (* whether it has been checked manually or reactively *)
         bool Eliom_shared.React.S.t >

val input_validation_tools :
   ?init:string (** The initial value, if existing *)
  -> ?set_focus:(bool -> unit) Eliom_client_value.t
  -> ?result_iter:
       (Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t -> string -> unit)
         Eliom_client_value.t
  -> ?invalid_class:string (** Default is "invalid" *)
  -> (string -> (string, string) Result.t) Eliom_shared.Value.t
  -> [> Html_types.input_attrib] Eliom_content.Html.attrib list
     * [> `Class] Eliom_content.Html.attrib
     * (string, string) Result.t Eliom_shared.React.S.t
(** Given an invalidation class and a validation function,
 ** builds the necessary [on_input], [on_blur]
 ** to give for your input for validation,
 ** and reactive classes for error states
 **
 ** Also is provided the signal on the error, if needed *)

[%%shared.start]

val disableable_button :
   ?a:[< Html_types.button_attrib] Eliom_content.Html.attrib list
  -> ?button_type:[< Eliom_form_sigs.button_type]
  -> disabled:bool Eliom_shared.React.S.t
  -> [< Html_types.button_content] Eliom_content.Html.elt list
  -> [> `Button] Eliom_content.Html.elt

val graceful_invalid_style : [`Input] Eliom_content.Html.elt -> unit
(** Adds .invalid class after each blur (un-focus). This allows us to
    style for invalidity without scaring the user from the very
    beginning. Somewhat inspired by :-moz-ui-invalid . *)

val optional_int_input :
   ?min:int
  -> ?max:int
  -> ?size:int
  -> int option
  -> [> `Div] Eliom_content.Html.elt
     * (int option, unit) result Eliom_shared.React.S.t

val int_input :
   ?min:int
  -> ?max:int
  -> ?size:int
  -> int
  -> [> `Div] Eliom_content.Html.elt * (int, unit) result Eliom_shared.React.S.t
