
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

```ocaml
val mk_service : string -> 'a Deriving_Json.t -> ('a, 'b) service
```
`mk_service name arg_deriver` Create a named service taking `(arg_deriver, (cropping, file))` parameter
