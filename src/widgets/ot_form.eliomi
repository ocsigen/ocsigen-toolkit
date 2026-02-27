(* Ocsigen
 * http://www.ocsigen.org
 *
 * Copyright (C) 2015 Vincent Balat
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

open%client Js_of_ocaml
open%shared Eliom_content.Html

(** {2 Reactive form widgets} *)

type%shared 'a react_component =
  'a Eliom_shared.React.S.t
  * (?step:React.step -> 'a -> unit) Eliom_shared.Value.t
(** A reactive component: a signal and its setter. *)

(** {3 Client-side utilities} *)

val%client resize_textarea : Dom_html.textAreaElement Js.t -> unit
(** Auto-resize a textarea element to fit its content. *)

val%client set_validity : Dom_html.element Js.t -> bool -> unit
(** [set_validity e b] adds or removes the ["ot-invalid"] class on [e]. *)

val%client valid : Dom_html.element Js.t -> bool
(** [valid e] checks HTML5 validity and absence of the ["ot-invalid"] class. *)

val%client set_custom_validity :
   Dom_html.element Js.t
  -> (string, string) result
  -> unit
(** [set_custom_validity e r] sets the HTML5 custom validity message on [e]. *)

val%client select_input_value : Dom_html.event Js.t -> unit
(** [select_input_value ev] selects the text content of the input
    that triggered [ev]. *)

val%client on_enter :
   f:(string -> unit Lwt.t)
  -> Dom_html.inputElement Js.t
  -> unit
(** [on_enter ~f inp] calls [f] with the input value when the user
    presses Enter, provided the input is valid. *)

(** {3 Buttons} *)

val%shared disableable_button :
   ?a:[< Html_types.button_attrib] attrib list
  -> ?button_type:[< Eliom_form_sigs.button_type]
  -> disabled:bool Eliom_shared.React.S.t
  -> [< Html_types.button_content] elt list
  -> [> `Button] elt
(** A button that can be reactively disabled. *)

(** {3 Radio buttons} *)

val%shared radio :
   ?a:[< Html_types.label_attrib] attrib list
  -> ?disabled_s:bool Eliom_shared.React.S.t
  -> ?checked_s:bool Eliom_shared.React.S.t
  -> ?name:string
  -> ?before:[< Html_types.label_content_fun > `Input `Span] elt list
  -> [< Html_types.span_content] elt list
  -> [> `Label] elt * [> `Input] elt
(** A single radio button with label. *)

val%shared radio_buttons :
   ?a:[< Html_types.label_attrib] attrib list
  -> ?disabled_s:int list Eliom_shared.React.S.t
  -> selection_react:int option react_component
  -> name:string
  -> [< Html_types.span_content] elt list list
  -> Html_types.div_content elt list
(** A group of radio buttons with reactive selection. *)

val%shared radio_selector :
   ?a:[< Html_types.div_attrib > `Class] attrib list
  -> selection_react:int option react_component
  -> name:string
  -> label:[< Html_types.div_content] elt list
  -> [< Html_types.span_content] elt list list
  -> [> `Div] elt
(** A styled container with label for a group of radio buttons. *)

(** {3 Reactive inputs} *)

val%shared reactify_input :
   ?input_r:string react_component
  -> ?value:string
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> [`Input | `Textarea] elt
  -> string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t
(** Make an existing input or textarea element reactive.
    Returns a signal tracking the current value and a setter. *)

val%shared reactive_input :
   ?a:[< Html_types.input_attrib] attrib list
  -> ?input_r:string react_component
  -> ?value:string
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> unit
  -> [> `Input] elt
     * (string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t)
(** Create a reactive text input. Returns the element and
    a (signal, setter) pair. *)

val%shared textarea :
   ?a:[< Html_types.textarea_attrib] attrib list
  -> ?a_rows:int
  -> ?resize:bool
  -> ?a_placeholder:string
  -> string
  -> [> `Textarea] elt
(** Create a textarea, optionally with auto-resize. *)

val%shared reactive_textarea :
   ?a:[< Html_types.textarea_attrib] attrib list
  -> ?a_rows:int
  -> ?resize:bool
  -> ?a_placeholder:string
  -> ?value:string
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> unit
  -> [> `Textarea] elt
     * (string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t)
(** Create a reactive textarea. *)

(** {3 Enter key binding} *)

val%shared lwt_bind_input_enter :
   ?validate:(string -> bool) Eliom_client_value.t
  -> ?button:[`Button] elt
  -> [`Input] elt
  -> (string -> unit Lwt.t) Eliom_client_value.t
  -> unit
(** Bind an Lwt action to an existing input, triggered on Enter key
    or optional button click. *)

val%shared lwt_bound_input_enter :
   ?a:[< Html_types.input_attrib] attrib list
  -> ?button:[`Button] elt
  -> ?validate:(string -> bool) Eliom_client_value.t
  -> (string -> unit Lwt.t) Eliom_client_value.t
  -> [> `Input] elt
(** Create an input with an Lwt action triggered on Enter key. *)

(** {3 Checkboxes} *)

type%shared checkbox_position = [`Left | `Right | `None]
type%shared checkbox_style = [`Box | `Bullet | `Small_bullet | `Toggle]

val%shared checkbox :
   ?a:[< Html_types.label_attrib] attrib list
  -> ?a_inp:[< Html_types.input_attrib] attrib list
  -> ?required:bool
  -> ?position:checkbox_position
  -> ?checked:bool
  -> ?readonly:bool
  -> ?disabled:bool
  -> ?style:checkbox_style
  -> ?mobile_style:checkbox_style
  -> [< Html_types.span_content] elt list
  -> [> `Label] elt * [> `Input] elt
(** A customizable checkbox with position and style options. *)

val%shared reactive_checkbox :
   ?a:[< Html_types.label_attrib] attrib list
  -> ?a_inp:[< Html_types.input_attrib] attrib list
  -> ?position:checkbox_position
  -> ?checked:bool
  -> ?readonly:bool
  -> ?disabled:bool
  -> ?style:checkbox_style
  -> ?mobile_style:checkbox_style
  -> ?ctrl:
       bool Eliom_shared.React.S.t
       * (?step:React.step -> bool -> unit) Eliom_shared.Value.t
  -> [< Html_types.span_content] elt list
  -> < label : [> `Label] elt
     ; input : [> `Input] elt
     ; value : bool Eliom_shared.React.S.t
     ; manually_changed : bool Eliom_shared.React.S.t >
(** A reactive checkbox. The returned object provides:
    - [label]: the label element
    - [input]: the input element
    - [value]: a signal tracking whether it is checked
    - [manually_changed]: a signal tracking whether the user changed it *)

(** {3 Validation} *)

val%shared input_validation_tools :
   ?init:string
  -> ?set_focus:(bool -> unit) Eliom_client_value.t
  -> ?result_iter:
       (Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t -> string -> unit)
         Eliom_client_value.t
  -> ?invalid_class:string
  -> (string -> (string, string) Result.t) Eliom_shared.Value.t
  -> [> Html_types.input_attrib] attrib list
     * [> `Class] attrib
     * (string, string) Result.t Eliom_shared.React.S.t
(** Given a validation function, builds the necessary [oninput] and [onblur]
    attributes for an input, a reactive class attribute for error states,
    and a signal with the validation result.

    The invalid class is only shown after the first blur (graceful). *)

val%shared graceful_invalid_style : [`Input] elt -> unit
(** Adds the ["ot-invalid"] class after each blur. This allows styling
    for invalidity without showing errors before the user interacts.
    Inspired by [:-moz-ui-invalid]. *)

val%shared validate_as_int : string -> (int option, unit) result
(** Validate a string as an optional integer. Returns [Ok None] for
    empty strings or ["-"], [Ok (Some n)] for valid integers,
    [Error ()] otherwise. *)

val%shared none_input_value : string
(** The string ["-"], used as placeholder for empty optional int inputs. *)

(** {2 Tab cycling (client-only)} *)

module%client Tabbable : sig
  (** An HTML element which can be selected by pressing the tab key. *)
  class type t = object
    inherit Dom_html.element
    method tabIndex : int Js.prop
  end
end

val%client setup_tabcycle : #Tabbable.t Js.t list -> unit
(** [setup_tabcycle] makes tab key loop over child elements of an element and
    only these elements. *)

val%client setup_tabcycle_auto : Dom_html.element Js.t -> unit
(** [setup_tabcycle_auto] scans an element for tabbable elements (buttons, inputs)
    and feeds them to [setup_tabcycle] *)

val%client setup_form : Dom_html.element Js.t -> unit
(** Scan for focusable elements, apply [setup_tabcycle_auto] to them and
    focus the first. *)

val%client prevent_tab : Dom_html.element Js.t -> unit -> unit
(** [prevent_tab e] prevents [e] (and its children) to be focused with tab key.
    A function to restore the initial status is returned. *)
