
# Module `Ot_picture_uploader`


### Picture uploader widget

`Ot_picture_uploader` allows the user to see a picture he wants to send to the before sending it server. Also, controllers can be added to allow the user to specify a cropping area. No cropping is actually done on the client side, it MUST be handled on server side.

```ocaml
type cropping = (float * float * float * float) React.S.t
```
```ocaml
type 'a upload =
  ?progress:(int -> int -> unit) ->
  ?cropping:cropping ->
  Js_of_ocaml.File.file Js_of_ocaml.Js.t ->
  'a Lwt.t
```
```ocaml
type ('a, 'b) service =
  (unit,
    'a * ((float * float * float * float) option * Eliom_lib.file_info),
    Eliom_service.post,
    Eliom_service.non_att,
    Eliom_service.co,
    Eliom_service.non_ext,
    Eliom_service.reg,
    [ `WithoutSuffix ],
    unit,
    [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name
    * ([ `One of (float * float * float * float) option Eliom_parameter.ocaml ]
         Eliom_parameter.param_name
       * [ `One of Eliom_lib.file_info ] Eliom_parameter.param_name),
    'b Eliom_service.ocaml)
    Eliom_service.t
```
a service that implements a function with type `'a -> 'b`

```ocaml
val ocaml_service_upload : service:('a, 'b) service -> arg:'a -> 'b upload
```
```ocaml
val cropper : 
  image:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t Eliom_client_value.t ->
  ?ratio:float ->
  unit ->
  (unit -> unit) * cropping * [> `Div ] Eliom_content.Html.elt
```
` let (reset, cropping, cropper_dom) = cropper ~image () ` ` reset ` is function to call to reset the current cropper status ` cropping ` are current cropping parameters ` cropper_dom ` is the div containing cropping controllers

```ocaml
val bind_input : 
  Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t Eliom_client_value.t ->
  Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t Eliom_client_value.t ->
  ?container:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t Eliom_client_value.t ->
  ?reset:(unit -> unit) ->
  unit ->
  unit
```
`bind_input input preview ?container ?reset ()` Bind `input` and `preview` so the file selected in `input` is the image displayed in `preview`. `?container` is used to toggle `ot-no-file` class. `?reset` is called when the `input` value changes.

```ocaml
val do_submit : 
  Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t Eliom_client_value.t ->
  ?progress:(int -> int -> unit) ->
  ?cropping:cropping ->
  upload:unit upload ->
  unit ->
  unit Lwt.t
```
` do_submit input ?cropping ~upload () ` `input` is the input with file loaded `cropping` are cropping info `upload` function to upload the file

```ocaml
val bind_submit : 
  Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t Eliom_client_value.t ->
  Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t Eliom_client_value.t ->
  ?cropping:cropping ->
  upload:unit upload ->
  after_submit:(unit -> unit Lwt.t) ->
  unit ->
  unit
```
` bind_submit input button ?cropping ~upload ~after_submit () ` binds the following two actions to ` button ` when it is being clicked: call ` do_submit ` which uploads the file; then call ` after_submit `

```ocaml
val bind : 
  ?container:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t Eliom_client_value.t ->
  input:Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t Eliom_client_value.t ->
  preview:Js_of_ocaml.Dom_html.imageElement Js_of_ocaml.Js.t ->
  ?crop:((unit -> unit) * cropping) ->
  submit:Js_of_ocaml.Dom_html.eventTarget Js_of_ocaml.Js.t Eliom_client_value.t ->
  upload:unit upload ->
  after_submit:(unit -> unit Lwt.t) ->
  unit ->
  unit
```
`bind` is a shortcut for `bind_input` and `bind_submit` actions

```ocaml
val input : 
  ?a:[< Html_types.label_attrib Class ] Eliom_content.Html.attrib list ->
  [< Html_types.label_content_fun ] Eliom_content.Html.elt list ->
  [> `Input ] Eliom_content.Html.elt * [> `Label ] Eliom_content.Html.elt
```
Create a file input element with good input type `ot-pup-input` class and wrap it into a label. Return (input node, label node)

```ocaml
val preview : 
  ?a:[< Html_types.img_attrib Class ] Eliom_content.Html.attrib list ->
  unit ->
  [> `Img ] Eliom_content.Html.elt
```
Create a img element with no src, no alt and `ot-pup-preview` class.

```ocaml
val submit : 
  ?a:[< Html_types.button_attrib Class ] Eliom_content.Html.attrib list ->
  [< Html_types.button_content ] Eliom_content.Html.elt list ->
  [> `Button ] Eliom_content.Html.elt
```
Create a button with `ot-pup-sumit` clas

```ocaml
val mk_form : 
  ?after_submit:(unit -> unit Lwt.t) ->
  ?crop:float option ->
  ?input:
    ([< Html_types.label_attrib Class ] Eliom_content.Html.attrib list
     * [< Html_types.label_content_fun ] Eliom_content.Html.elt list) ->
  ?submit:
    ([< Html_types.button_attrib Class ] Eliom_content.Html.attrib list
     * [< Html_types.button_content_fun ] Eliom_content.Html.elt list) ->
  unit upload ->
  [> `Form ] Eliom_content.Html.elt Lwt.t
```
Ready-to-use form. Customizable with `input`, the input button content, `submit`, the submit button content. If `crop` is present, cropping is enable, with the optional ratio it is. The last argument determines the method by which the file is uploaded.
