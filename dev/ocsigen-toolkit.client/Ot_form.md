
# Module `Ot_form`

```ocaml
type 'a react_component =
  'a Eliom_shared.React.S.t
  * (?step:React.step -> 'a -> unit) Eliom_shared.Value.t
```
A reactive component: a signal and its setter.

```ocaml
val resize_textarea : 
  Js_of_ocaml.Dom_html.textAreaElement Js_of_ocaml.Js.t ->
  unit
```
Auto-resize a textarea element to fit its content.

```ocaml
val set_validity : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  bool ->
  unit
```
`set_validity e b` adds or removes the `"ot-invalid"` class on `e`.

```ocaml
val valid : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> bool
```
`valid e` checks HTML5 validity and absence of the `"ot-invalid"` class.

```ocaml
val set_custom_validity : 
  Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t ->
  (string, string) result ->
  unit
```
`set_custom_validity e r` sets the HTML5 custom validity message on `e`.

```ocaml
val select_input_value : Js_of_ocaml.Dom_html.event Js_of_ocaml.Js.t -> unit
```
`select_input_value ev` selects the text content of the input that triggered `ev`.

```ocaml
val on_enter : 
  f:(string -> unit Lwt.t) ->
  Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t ->
  unit
```
`on_enter ~f inp` calls `f` with the input value when the user presses Enter, provided the input is valid.

```ocaml
val disableable_button : 
  ?a:[< Html_types.button_attrib ] Eliom_content.Html.attrib list ->
  ?button_type:[< Eliom_form_sigs.button_type ] ->
  disabled:bool Eliom_shared.React.S.t ->
  [< Html_types.button_content ] Eliom_content.Html.elt list ->
  [> `Button ] Eliom_content.Html.elt
```
A button that can be reactively disabled.

```ocaml
val reactive_toggle_button : 
  ?a:[< Html_types.button_attrib ] Eliom_content.Html.attrib list ->
  ?init:bool ->
  ?ctrl:bool react_component ->
  [< Html_types.button_content ] Eliom_content.Html.elt list ->
  [> `Button ] Eliom_content.Html.elt * bool react_component
```
A toggle button that alternates between on/off states. The CSS classes `"ot-toggle-on"` and `"ot-toggle-off"` are set reactively on the button element.

```ocaml
val radio : 
  ?a:[< Html_types.label_attrib ] Eliom_content.Html.attrib list ->
  ?disabled_s:bool Eliom_shared.React.S.t ->
  ?checked_s:bool Eliom_shared.React.S.t ->
  ?name:string ->
  ?before:
    [< Html_types.label_content_fun Input Span ] Eliom_content.Html.elt list ->
  [< Html_types.span_content ] Eliom_content.Html.elt list ->
  [> `Label ] Eliom_content.Html.elt * [> `Input ] Eliom_content.Html.elt
```
A single radio button with label.

```ocaml
val radio_buttons : 
  ?a:[< Html_types.label_attrib ] Eliom_content.Html.attrib list ->
  ?disabled_s:int list Eliom_shared.React.S.t ->
  selection_react:int option react_component ->
  name:string ->
  [< Html_types.span_content ] Eliom_content.Html.elt list list ->
  Html_types.div_content Eliom_content.Html.elt list
```
A group of radio buttons with reactive selection.

```ocaml
val radio_selector : 
  ?a:[< Html_types.div_attrib Class ] Eliom_content.Html.attrib list ->
  selection_react:int option react_component ->
  name:string ->
  label:[< Html_types.div_content ] Eliom_content.Html.elt list ->
  [< Html_types.span_content ] Eliom_content.Html.elt list list ->
  [> `Div ] Eliom_content.Html.elt
```
A styled container with label for a group of radio buttons.

```ocaml
val reactify_input : 
  ?input_r:string react_component ->
  ?value:string ->
  ?validate:(string -> bool) Eliom_client_value.t ->
  [ `Input | `Textarea ] Eliom_content.Html.elt ->
  string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t
```
Make an existing input or textarea element reactive. Returns a signal tracking the current value and a setter.

```ocaml
val reactive_input : 
  ?a:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?input_r:string react_component ->
  ?value:string ->
  ?validate:(string -> bool) Eliom_client_value.t ->
  unit ->
  [> `Input ] Eliom_content.Html.elt
  * (string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t)
```
Create a reactive text input. Returns the element and a (signal, setter) pair.

```ocaml
val textarea : 
  ?a:[< Html_types.textarea_attrib ] Eliom_content.Html.attrib list ->
  ?a_rows:int ->
  ?resize:bool ->
  ?a_placeholder:string ->
  string ->
  [> `Textarea ] Eliom_content.Html.elt
```
Create a textarea, optionally with auto-resize.

```ocaml
val reactive_textarea : 
  ?a:[< Html_types.textarea_attrib ] Eliom_content.Html.attrib list ->
  ?a_rows:int ->
  ?resize:bool ->
  ?a_placeholder:string ->
  ?value:string ->
  ?validate:(string -> bool) Eliom_client_value.t ->
  unit ->
  [> `Textarea ] Eliom_content.Html.elt
  * (string Eliom_shared.React.S.t * (string -> unit) Eliom_client_value.t)
```
Create a reactive textarea.

```ocaml
val debounced_input : 
  ?a:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?delay:float ->
  ?value:string ->
  ?validate:(string -> bool) Eliom_client_value.t ->
  unit ->
  [> `Input ] Eliom_content.Html.elt
  * (string Eliom_shared.React.S.t
     * string Eliom_shared.React.S.t
     * (string -> unit) Eliom_client_value.t)
```
A reactive input where the debounced signal updates only after `delay` seconds (default 0\.3) of inactivity. Returns `(input, (raw_signal, debounced_signal, setter))`. `raw_signal` updates on every keystroke; `debounced_signal` waits for the user to stop typing.

```ocaml
val lwt_bind_input_enter : 
  ?validate:(string -> bool) Eliom_client_value.t ->
  ?button:[ `Button ] Eliom_content.Html.elt ->
  [ `Input ] Eliom_content.Html.elt ->
  (string -> unit Lwt.t) Eliom_client_value.t ->
  unit
```
Bind an Lwt action to an existing input, triggered on Enter key or optional button click.

```ocaml
val lwt_bound_input_enter : 
  ?a:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?button:[ `Button ] Eliom_content.Html.elt ->
  ?validate:(string -> bool) Eliom_client_value.t ->
  (string -> unit Lwt.t) Eliom_client_value.t ->
  [> `Input ] Eliom_content.Html.elt
```
Create an input with an Lwt action triggered on Enter key.

```ocaml
type checkbox_position = [ 
  | `Left
  | `Right
  | `None
 ]
```
```ocaml
type checkbox_style = [ 
  | `Box
  | `Bullet
  | `Small_bullet
  | `Toggle
 ]
```
```ocaml
val checkbox : 
  ?a:[< Html_types.label_attrib ] Eliom_content.Html.attrib list ->
  ?a_inp:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?required:bool ->
  ?position:checkbox_position ->
  ?checked:bool ->
  ?readonly:bool ->
  ?disabled:bool ->
  ?style:checkbox_style ->
  ?mobile_style:checkbox_style ->
  [< Html_types.span_content ] Eliom_content.Html.elt list ->
  [> `Label ] Eliom_content.Html.elt * [> `Input ] Eliom_content.Html.elt
```
A customizable checkbox with position and style options.

```ocaml
val reactive_checkbox : 
  ?a:[< Html_types.label_attrib ] Eliom_content.Html.attrib list ->
  ?a_inp:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?position:checkbox_position ->
  ?checked:bool ->
  ?readonly:bool ->
  ?disabled:bool ->
  ?style:checkbox_style ->
  ?mobile_style:checkbox_style ->
  ?ctrl:
    (bool Eliom_shared.React.S.t
     * (?step:React.step -> bool -> unit) Eliom_shared.Value.t) ->
  [< Html_types.span_content ] Eliom_content.Html.elt list ->
  < label : [> `Label ] Eliom_content.Html.elt
  ; input : [> `Input ] Eliom_content.Html.elt
  ; value : bool Eliom_shared.React.S.t
  ; manually_changed : bool Eliom_shared.React.S.t >
```
A reactive checkbox. The returned object provides:

- `label`: the label element
- `input`: the input element
- `value`: a signal tracking whether it is checked
- `manually_changed`: a signal tracking whether the user changed it
```ocaml
val input_validation_tools : 
  ?init:string ->
  ?set_focus:(bool -> unit) Eliom_client_value.t ->
  ?result_iter:
    (Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t ->
      string ->
      unit)
      Eliom_client_value.t ->
  ?invalid_class:string ->
  (string -> (string, string) Result.t) Eliom_shared.Value.t ->
  [> Html_types.input_attrib ] Eliom_content.Html.attrib list
  * [> `Class ] Eliom_content.Html.attrib
  * (string, string) Result.t Eliom_shared.React.S.t
```
Given a validation function, builds the necessary `oninput` and `onblur` attributes for an input, a reactive class attribute for error states, and a signal with the validation result.

The invalid class is only shown after the first blur (graceful).

```ocaml
val graceful_invalid_style : [ `Input ] Eliom_content.Html.elt -> unit
```
Adds the `"ot-invalid"` class after each blur. This allows styling for invalidity without showing errors before the user interacts. Inspired by `:-moz-ui-invalid`.

```ocaml
val optional_int_input : 
  ?min:int ->
  ?max:int ->
  ?size:int ->
  int option ->
  [> `Div ] Eliom_content.Html.elt
  * (int option, unit) result Eliom_shared.React.S.t
```
An integer input with \+/- buttons that can be empty (None). Displays `"-"` when empty.

```ocaml
val int_input : 
  ?min:int ->
  ?max:int ->
  ?size:int ->
  int ->
  [> `Div ] Eliom_content.Html.elt * (int, unit) result Eliom_shared.React.S.t
```
An integer input with \+/- buttons. Always contains a value.

```ocaml
val password_input : 
  ?a:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?placeholder:string ->
  unit ->
  [> `Div ] Eliom_content.Html.elt
  * [> `Input ] Eliom_content.Html.elt
  * bool react_component
```
A password input with a visibility toggle button. Returns `(container, input, (visible_signal, set_visible))`. The toggle switches between `type=password` and `type=text`. CSS classes: `ot-password-container`, `ot-password-input`, `ot-password-toggle`, `ot-password-toggle-show`, `ot-password-toggle-hide`.

```ocaml
val password_toggle : 
  [< Html_types.input ] Eliom_content.Html.elt ->
  [> `Div ] Eliom_content.Html.elt
```
`password_toggle inp` wraps an existing password input element `inp` in a container with a visibility toggle button. Unlike [`password_input`](./#val-password_input), this does not use reactive signals: it manipulates the DOM directly on click, making it suitable for use with `D.Form.input` in Eliom forms. CSS classes: `ot-password-container`, `ot-password-toggle`, `ot-password-toggle-show`, `ot-password-toggle-hide`.

```ocaml
val prevent_double_submit : 
  ?a:[< Html_types.button_attrib ] Eliom_content.Html.attrib list ->
  ?button_type:[< Eliom_form_sigs.button_type ] ->
  f:(unit -> unit Lwt.t) Eliom_client_value.t ->
  [< Html_types.button_content ] Eliom_content.Html.elt list ->
  [> `Button ] Eliom_content.Html.elt
```
A button that disables itself while the action `f` is running, preventing double submissions.

```ocaml
val reactive_select : 
  ?a:[< Html_types.select_attrib ] Eliom_content.Html.attrib list ->
  options:(string * string) list ->
  ?selected:string ->
  unit ->
  [> `Select ] Eliom_content.Html.elt * string react_component
```
`reactive_select ~options ()` creates a `<select>` element with reactive selection tracking. `options` is a list of `(value, label)` pairs. Returns the element and a `(signal, setter)` pair. The setter can be used to change the selection programmatically.

```ocaml
val validate_as_int : string -> (int option, unit) result
```
Validate a string as an optional integer. Returns `Ok None` for empty strings or `"-"`, `Ok (Some n)` for valid integers, `Error ()` otherwise.

```ocaml
val none_input_value : string
```
The string `"-"`, used as placeholder for empty optional int inputs.

```ocaml
val reactive_fieldset : 
  ?a:[< Html_types.fieldset_attrib ] Eliom_content.Html.attrib list ->
  disabled:bool Eliom_shared.React.S.t ->
  [< Html_types.fieldset_content ] Eliom_content.Html.elt list ->
  [> `Fieldset ] Eliom_content.Html.elt
```
A fieldset that can be reactively disabled. When disabled, all form elements inside are disabled by the browser.

```ocaml
val parse_date : string -> (int * int * int) option
```
Parse a date string in `YYYY-MM-DD` format.

```ocaml
val parse_time : string -> (int * int) option
```
Parse a time string in `HH:MM` format.

```ocaml
val string_of_date : (int * int * int) -> string
```
Format a `(year, month, day)` triple as `YYYY-MM-DD`.

```ocaml
val string_of_time : (int * int) -> string
```
Format a `(hours, minutes)` pair as `HH:MM`.

```ocaml
val reactive_date_input : 
  ?a:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?value:(int * int * int) ->
  unit ->
  [> `Input ] Eliom_content.Html.elt * (int * int * int) option react_component
```
A reactive HTML5 date input. The signal carries `Some (year, month, day)` or `None` when empty.

```ocaml
val reactive_time_input : 
  ?a:[< Html_types.input_attrib ] Eliom_content.Html.attrib list ->
  ?value:(int * int) ->
  unit ->
  [> `Input ] Eliom_content.Html.elt * (int * int) option react_component
```
A reactive HTML5 time input. The signal carries `Some (hours, minutes)` or `None` when empty.

```ocaml
module Tabbable : sig ... end
```
```ocaml
val setup_tabcycle : Tabbable.t Js_of_ocaml.Js.t list -> unit
```
`setup_tabcycle` makes tab key loop over child elements of an element and only these elements.

```ocaml
val setup_tabcycle_auto : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit
```
`setup_tabcycle_auto` scans an element for tabbable elements (buttons, inputs) and feeds them to `setup_tabcycle`

```ocaml
val setup_form : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit
```
Scan for focusable elements, apply `setup_tabcycle_auto` to them and focus the first.

```ocaml
val prevent_tab : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> unit -> unit
```
`prevent_tab e` prevents `e` (and its children) to be focused with tab key. A function to restore the initial status is returned.
