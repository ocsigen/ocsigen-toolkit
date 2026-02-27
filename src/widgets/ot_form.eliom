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
open%client Js_of_ocaml_lwt
open%shared Eliom_content.Html
open%shared Eliom_content.Html.F
open%client Lwt.Syntax

(* ================================================================ *)
(* Reactive form widgets                                            *)
(* ================================================================ *)

type%shared 'a react_component =
  'a Eliom_shared.React.S.t
  * (?step:React.step -> 'a -> unit) Eliom_shared.Value.t

let%shared cons_opt opt l = match opt with Some x -> x :: l | None -> l

(* -- Client utilities -------------------------------------------- *)

let%client resize_textarea (elt : Dom_html.textAreaElement Js.t) =
  elt##.style##.overflow := Js.string "hidden";
  elt##.style##.height := Js.string "auto";
  elt##.style##.height := Js.string (string_of_int elt##.scrollHeight ^ "px")

let%client set_validity e b =
  let class_ = Js.string "ot-invalid" in
  if b then e##.classList##remove class_ else e##.classList##add class_

let%client valid e =
  try
    (Js.Unsafe.coerce e)##checkValidity
    && not (Js.to_bool (e##.classList##contains (Js.string "ot-invalid")))
  with _ -> true

let%client set_custom_validity inp r =
  ignore
  @@
  match r with
  | Ok _ -> (Js.Unsafe.coerce inp)##setCustomValidity (Js.string "")
  | Error s -> (Js.Unsafe.coerce inp)##setCustomValidity (Js.string s)

let%client select_input_value ev =
  Js.Opt.iter ev##.currentTarget @@ fun input ->
  match Dom_html.tagged input with
  | Dom_html.Input input -> input##select
  | _ -> ()

let%client on_enter ~f inp =
  Lwt.async @@ fun () ->
  Lwt_js_events.keydowns inp @@ fun ev _ ->
  if ev##.keyCode = 13 && valid inp
  then f (Js.to_string inp##.value)
  else Lwt.return_unit

(* -- Disableable button ------------------------------------------ *)

let%shared disableable_button ?(a = []) ?button_type ~disabled content =
  let button_type =
    match button_type with
    | Some b -> (b :> Eliom_form_sigs.button_type)
    | None -> `Button
  in
  let a = (a :> Html_types.button_attrib attrib list) in
  Form.button_no_value
    ~a:(R.filter_attrib (a_disabled ()) disabled :: a)
    ~button_type
    (content :> Html_types.button_content elt list)

(* -- Toggle button ----------------------------------------------- *)

let%shared
    reactive_toggle_button
      ?(a = [])
      ?(init = false)
      ?(ctrl = Eliom_shared.React.S.create init)
      content
  =
  let signal, set_signal = ctrl in
  let elt =
    D.button
      ~a:
        (a_onclick
           [%client
             fun ev ->
               Dom_html.stopPropagation ev;
               Dom.preventDefault ev;
               ~%set_signal (not (React.S.value ~%signal))]
        :: a_button_type `Button
        :: R.a_class
             (Eliom_shared.React.S.map
                [%shared
                  fun checked ->
                    "ot-toggle-button"
                    :: (if checked then ["ot-toggle-on"] else ["ot-toggle-off"])]
                signal)
        :: (a :> Html_types.button_attrib attrib list))
      content
  in
  elt, (signal, set_signal)

(* -- Radio buttons ----------------------------------------------- *)

let%shared
    radio
      ?(a = [])
      ?(disabled_s = Eliom_shared.React.S.const false)
      ?(checked_s = Eliom_shared.React.S.const false)
      ?name
      ?(before = [])
      content
  =
  let a_checked =
    R.filter_attrib (a_checked ())
    @@ Eliom_shared.React.S.l2
         [%shared fun checked disabled -> if disabled then false else checked]
         checked_s disabled_s
  in
  let a_disabled = R.filter_attrib (a_disabled ()) disabled_s in
  let a' = [a_disabled; a_checked] in
  let a' = a_class ["ot-radio-input"] :: a_input_type `Radio :: a' in
  let e =
    D.input ~a:(match name with Some v -> a_name v :: a' | None -> a') ()
  in
  let l =
    D.label
      ~a:(a_class ["ot-radio"] :: (a :> Html_types.label_attrib attrib list))
      (before @ [span ~a:[a_class ["ot-radio-label"]] content; e])
  in
  l, e

let%shared
    radio_buttons
      ?(a = [])
      ?(disabled_s =
        (Eliom_shared.React.S.const [] : int list Eliom_shared.React.S.t))
      ~(selection_react : int option react_component)
      ~name
      contents
  =
  let a = (a :> Html_types.label_attrib attrib list) in
  let labels = ref [] in
  let s, set = selection_react in
  let initial = Eliom_shared.React.S.value s in
  let mk_radio i content =
    let checked = Option.map (( = ) i) (Eliom_shared.Value.local initial) in
    let cl =
      R.a_class
      @@ Eliom_shared.React.S.map
           [%shared
             fun disabled ->
               if List.mem ~%i disabled
               then ["disabled"]
               else if ~%checked = Some true
               then ["checked"]
               else []]
           disabled_s
    in
    let a = cl :: a in
    let disabled_s =
      Eliom_shared.React.S.map
        [%shared fun disabled -> List.mem ~%i disabled]
        disabled_s
    in
    let label, input =
      radio ~a ~disabled_s
        ?checked_s:(Option.map Eliom_shared.React.S.const checked)
        ~name content
    in
    ignore
      [%client
        (Lwt.async @@ fun () ->
         let input = To_dom.of_input ~%input in
         Lwt_js_events.changes input @@ fun _ _ ->
         if Js.to_bool input##.checked
         then (
           (match Eliom_shared.React.S.value ~%s with
           | None -> ()
           | Some p -> Manip.Class.remove (List.nth !(~%labels) p) "checked");
           Manip.Class.add ~%label "checked";
           ~%set (Some ~%i));
         Lwt.return_unit
         : unit)];
    label
  in
  labels := List.mapi mk_radio contents;
  !labels

let%shared radio_selector ?(a = []) ~selection_react ~name ~label choices =
  let buttons = radio_buttons ~name ~selection_react choices in
  F.div
    ~a:(F.a_class ["ot-radio-selector-container"] :: a)
    [ F.div ~a:[F.a_class ["ot-radio-selector-label"]] label
    ; F.div ~a:[F.a_class ["ot-radio-selector"]] buttons ]

(* -- Reactive inputs --------------------------------------------- *)

let%shared
    reactify_input
      ?(input_r : string react_component option)
      ?(value = "")
      ?(validate : (string -> bool) Eliom_client_value.t option)
      (e : [`Input | `Textarea] elt)
  =
  let signal, set_signal =
    match input_r with Some r -> r | None -> Eliom_shared.React.S.create value
  in
  let e =
    [%client
      (To_dom.of_element ~%e : Js_of_ocaml.Dom_html.element Js_of_ocaml__.Js.t)]
  in
  let e_with_value =
    [%client
      ((match Dom_html.tagged ~%e with
       | Dom_html.Input e -> (e :> < value : Js.js_string Js.t Js.prop > Js.t)
       | Dom_html.Textarea e ->
           (e :> < value : Js.js_string Js.t Js.prop > Js.t)
       | _ -> assert false)
       : < value :
             Js_of_ocaml__.Js.js_string Js_of_ocaml__.Js.t Js_of_ocaml__.Js.prop >
           Js_of_ocaml__.Js.t)]
  in
  let set_signal =
    [%client
      (Eliom_lib.Dom_reference.retain ~%e
         ~keep:
           (React.S.map
              (fun s ->
                 if Js.to_string ~%e_with_value##.value <> s
                 then ~%e_with_value##.value := Js.string s)
              ~%signal);
       (match ~%validate with
       | Some f ->
           Eliom_lib.Dom_reference.retain ~%e
             ~keep:(React.S.map (fun x -> set_validity ~%e (f x)) ~%signal)
       | None -> ());
      
       let f _ _ =
         let v = Js.to_string ~%e_with_value##.value in
         ~%set_signal v; Lwt.return_unit
       in
       Lwt.async (fun () -> Lwt_js_events.inputs ~%e f);
       fun value -> ~%set_signal value
       : _ -> _)]
  in
  signal, set_signal

let%shared
    reactive_input
      ?(a = [])
      ?(input_r : string react_component option)
      ?value
      ?validate
      ()
  =
  let a = (a :> Html_types.input_attrib attrib list) in
  let e =
    D.Raw.input ~a:(match value with Some v -> a_value v :: a | None -> a) ()
  in
  let signal = reactify_input ?input_r ?value ?validate e in
  e, signal

let%shared
    textarea
      ?(a = [])
      ?a_rows:(rows = 4)
      ?(resize = false)
      ?a_placeholder:(placeholder = "")
      value
  =
  let elt_ref = ref @@ D.Raw.textarea @@ txt value in
  let resize_cb =
    [%client
      fun e ->
        Js.Opt.iter e##.currentTarget @@ fun target ->
        match Dom_html.tagged target with
        | Dom_html.Textarea elt -> resize_textarea elt
        | _ -> ()]
  in
  let resize_onload =
    [%client
      fun _ ->
        Lwt.async @@ fun () ->
        let* () = Lwt_js_events.request_animation_frame () in
        resize_textarea @@ To_dom.of_textarea !(~%elt_ref);
        Lwt.return_unit]
  in
  let resize_onload = if resize then Some (a_onload resize_onload) else None in
  let resize_oninput = if resize then Some (a_oninput resize_cb) else None in
  let a = (a :> Html_types.textarea_attrib Eliom_content.Html.attrib list) in
  let ta =
    D.Raw.textarea
      ~a:
        (a_class ["ot-form-input"; "ot-form-textarea"]
        :: a_rows rows :: a_placeholder placeholder
        :: cons_opt resize_oninput (cons_opt resize_onload a))
      (txt value)
  in
  elt_ref := ta;
  ta

let%shared
    reactive_textarea
      ?(a = [])
      ?a_rows
      ?resize
      ?a_placeholder
      ?value
      ?validate
      ()
  =
  let a = (a :> Html_types.textarea_attrib attrib list) in
  let e =
    textarea ~a ?a_rows ?resize ?a_placeholder
      (match value with Some v -> v | None -> "")
  in
  let signal = reactify_input ?value ?validate e in
  e, signal

(* -- Enter key binding ------------------------------------------- *)

let%shared
    lwt_bind_input_enter
      ?(validate : (string -> bool) Eliom_client_value.t option)
      ?button
      (e : Html_types.input elt)
      (f : (string -> unit Lwt.t) Eliom_client_value.t)
  =
  ignore
    [%client
      (let e = To_dom.of_input ~%e in
       let f =
         let f = ~%(f : (string -> unit Lwt.t) Eliom_client_value.t) in
         match ~%validate with
         | Some validate ->
             fun v ->
               set_validity e (validate v);
               f v
         | None -> f
       in
       on_enter ~f e;
       match
         ~%(button : [< Html_types.button | Html_types.input] elt option)
       with
       | Some button ->
           Lwt.async @@ fun () ->
           Lwt_js_events.clicks (To_dom.of_element button) @@ fun _ _ ->
           if valid e then f (Js.to_string e##.value) else Lwt.return_unit
       | None -> ()
       : unit)]

let%shared lwt_bound_input_enter ?(a = []) ?button ?validate f =
  let e = D.Raw.input ~a () in
  lwt_bind_input_enter ?button ?validate e f;
  e

(* -- Checkboxes -------------------------------------------------- *)

type%shared checkbox_position = [`Left | `Right | `None]
type%shared checkbox_style = [`Box | `Bullet | `Small_bullet | `Toggle]

let%shared
    checkbox
      ?(a = [])
      ?(a_inp = [])
      ?(required = false)
      ?(position = (`Right :> checkbox_position))
      ?(checked = false)
      ?(readonly = false)
      ?(disabled = false)
      ?(style = `Box)
      ?(mobile_style : checkbox_style option)
      content
  =
  let mobile_style =
    match mobile_style with
    | None -> if style = `Box then `Toggle else style
    | Some s -> s
  in
  let inp =
    D.Raw.input
      ~a:
        ((if readonly || disabled then [a_disabled ()] else [])
        @ (if required then [a_required ()] else [])
        @ a_class ["ot-checkbox-input"]
          :: a_input_type `Checkbox
          :: (if checked then [a_checked ()] else [])
        @ (a_inp :> Html_types.input_attrib attrib list))
      ()
  in
  let box =
    let disabled_class = if disabled then Some "disabled" else None in
    let style_classes_desktop =
      match style with
      | `Small_bullet ->
          ["ot-checkbox-bullet-desktop"; "ot-small-checkbox-bullet"]
      | `Bullet -> ["ot-checkbox-bullet-desktop"]
      | `Box -> ["ot-checkbox-box-desktop"]
      | `Toggle -> ["ot-checkbox-toggle-desktop"]
    in
    let style_classes_mobile =
      match mobile_style with
      | `Small_bullet ->
          ["ot-checkbox-bullet-mobile"; "ot-small-checkbox-bullet"]
      | `Bullet -> ["ot-checkbox-bullet-mobile"]
      | `Box -> ["ot-checkbox-box-mobile"]
      | `Toggle -> ["ot-checkbox-toggle-mobile"]
    in
    let style_classes = style_classes_desktop @ style_classes_mobile in
    let position_class =
      match position with
      | `Left -> Some "ot-checkbox-left"
      | `Right -> Some "ot-checkbox-right"
      | `None -> None
    in
    let checkbox_classes =
      cons_opt disabled_class ("ot-checkbox" :: style_classes)
    in
    let label_classes =
      match position_class with
      | Some c -> ["ot-checkbox-label"; c]
      | None -> ["ot-checkbox-label"]
    in
    label
      ~a:(a_class checkbox_classes :: (a :> Html_types.label_attrib attrib list))
      [ inp
      ; span
          ~a:[a_class label_classes]
          [ (span
               ~a:[a_class ["ot-checkbox-decoration"]]
               [span ~a:[a_class ["ot-checkbox-sub-decoration"]] []]
              :> [< Html_types.span_content] Eliom_content.Html.elt)
          ; span content ] ]
  in
  box, inp

let%shared
    reactive_checkbox
      ?a
      ?a_inp
      ?position
      ?(checked = false)
      ?readonly
      ?disabled
      ?style
      ?mobile_style
      ?(ctrl = Eliom_shared.React.S.create checked)
      content
  =
  let box, inp =
    checkbox ?a ?a_inp ~checked ?readonly ?position ?disabled ?style
      ?mobile_style content
  in
  let signal, set_signal = ctrl in
  let manually_changed, set_manually_changed =
    Eliom_shared.React.S.create false
  in
  let (_ : unit Eliom_client_value.t) =
    [%client
      let disabled = Option.value ~default:false ~%disabled in
      if not disabled
      then
        let inp' = To_dom.of_input ~%inp in
        Eliom_lib.Dom_reference.retain inp'
          ~keep:
            (Eliom_shared.React.S.map
               (fun b -> inp'##.checked := Js.bool b)
               ~%signal);
        Lwt.async (fun () ->
          Lwt_js_events.changes inp' (fun _ _ ->
            ~%set_manually_changed true;
            ~%set_signal (Js.to_bool inp'##.checked);
            Lwt.return_unit))]
  in
  object
    method label = box
    method input = inp
    method value = signal
    method manually_changed = manually_changed
  end

(* -- Validation -------------------------------------------------- *)

let%shared
    input_validation_tools
      ?init
      ?set_focus
      ?(result_iter :
         (Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t -> string -> unit)
           Eliom_client_value.t
           option)
      ?(invalid_class = "ot-invalid")
      (check : (string -> (string, string) Result.t) Eliom_shared.Value.t)
  =
  let invalid_cl valid_s lazy_invalid_s =
    R.filter_attrib (a_class [invalid_class])
    @@ Eliom_shared.React.S.l2
         [%shared fun valid lazy_invalid -> (not valid) && lazy_invalid]
         valid_s lazy_invalid_s
  in
  let result_s, set_result =
    Eliom_shared.React.S.create
    @@
    match init with
    | Some i -> (Eliom_shared.Value.local check) i
    | None -> Ok ""
  in
  let init_valid =
    match init with
    | Some i -> Result.is_ok @@ (Eliom_shared.Value.local check) i
    | None -> true
  in
  let valid_s, set_valid = Eliom_shared.React.S.create init_valid in
  let init_lazy_invalid =
    match init with
    | Some i -> Result.is_error @@ (Eliom_shared.Value.local check) i
    | None -> false
  in
  let lazy_invalid_s, set_lazy_invalid =
    Eliom_shared.React.S.create init_lazy_invalid
  in
  let a_oninput_attr =
    a_oninput
      [%client
        fun ev ->
          Js.Opt.iter ev##.currentTarget @@ fun target ->
          match Dom_html.tagged target with
          | Dom_html.Input t ->
              let r = ~%check (Js.to_string t##.value) in
              ~%set_result r;
              ~%set_valid (Result.is_ok r)
          | _ -> ()]
  in
  let a_onfocus_o =
    Option.map
      (fun set_focus -> a_onfocus [%client fun _ -> ~%set_focus true])
      set_focus
  in
  let a_onblur_attr =
    a_onblur
      [%client
        fun ev ->
          Js.Opt.iter ev##.currentTarget @@ fun target ->
          match Dom_html.tagged target with
          | Dom_html.Input t ->
              let r = ~%check (Js.to_string t##.value) in
              Option.iter (fun f -> Result.iter (f t) r) ~%result_iter;
              Option.iter (fun f -> f false) ~%set_focus;
              ~%set_result r;
              set_custom_validity t r;
              ~%set_lazy_invalid (Result.is_error r)
          | _ -> ()]
  in
  ( cons_opt a_onfocus_o [a_onblur_attr; a_oninput_attr]
  , invalid_cl valid_s lazy_invalid_s
  , result_s )

let%shared
    graceful_invalid_style (inp : Html_types.input Eliom_content.Html.elt)
  =
  ignore
  @@ [%client
       (let inp = Eliom_content.Html.To_dom.of_input ~%inp in
        let f () = set_validity inp (Js.Unsafe.coerce inp)##checkValidity in
        Lwt.async @@ fun () ->
        let* _ = Lwt_js_events.blur inp in
        f ();
        Lwt_js_events.inputs inp @@ fun _ _ -> f (); Lwt.return_unit
        : unit)]

(* -- Reactive select --------------------------------------------- *)

let%shared reactive_select ?(a = []) ~options ?selected () =
  let initial =
    match selected with Some v -> v | None -> fst (List.hd options)
  in
  let signal, set_signal = Eliom_shared.React.S.create initial in
  let make_option (value, label_text) =
    let a_sel = if value = initial then [a_selected ()] else [] in
    F.option ~a:(a_value value :: a_sel) (F.txt label_text)
  in
  let elt =
    D.Raw.select
      ~a:
        (a_class ["ot-form-select"]
        :: (a :> Html_types.select_attrib attrib list))
      (List.map make_option options)
  in
  let (_ : unit Eliom_client_value.t) =
    [%client
      let dom =
        (Js.Unsafe.coerce (To_dom.of_element ~%elt)
         : < value : Js.js_string Js.t Js.prop > Js.t)
      in
      Lwt.async (fun () ->
        Lwt_js_events.changes (To_dom.of_element ~%elt) @@ fun _ _ ->
        ~%set_signal (Js.to_string dom##.value);
        Lwt.return_unit);
      Eliom_lib.Dom_reference.retain (To_dom.of_element ~%elt)
        ~keep:
          (React.S.map
             (fun v ->
                if Js.to_string dom##.value <> v then dom##.value := Js.string v)
             ~%signal)]
  in
  elt, (signal, set_signal)

(* -- Integer inputs ---------------------------------------------- *)

let%shared none_input_value = "-"

let%shared validate_as_int value =
  let value = String.trim value in
  if String.length value = 0 || value = none_input_value
  then Ok None
  else
    match int_of_string_opt value with
    | None -> Error ()
    | Some value -> Ok (Some value)

let%shared
    int_step_button
      ~min_value
      ~max_value
      ~value
      ~set_value
      ~input_elt
      ~optional
      step
  =
  let disabled =
    Eliom_shared.React.S.map
      [%shared
        function
        | Error () -> true
        | Ok None -> ~%step < 0
        | Ok (Some v) ->
            if ~%step < 0
            then if ~%optional then v < ~%min_value else v <= ~%min_value
            else v >= ~%max_value]
      value
  in
  let execute_step =
    [%client
      fun _ ->
        let value =
          match Eliom_shared.React.S.value ~%value with
          | Ok None | Error () -> pred ~%min_value
          | Ok (Some value) -> value
        in
        let s =
          if ~%step < 0 && value = ~%min_value && ~%optional
          then none_input_value
          else string_of_int @@ ( + ) ~%step @@ value
        in
        ~%set_value s;
        (To_dom.of_input ~%input_elt)##.value := Js.string s]
  in
  disableable_button
    ~a:[F.a_onclick execute_step; F.a_class ["ot-form-step-button"]]
    ~disabled
    [F.txt (if step < 0 then "\xe2\x88\x92" else "+")]

let%shared make_int_input ~min ~max ~size ~optional initial_value =
  let input_r =
    let initial_value =
      match initial_value with
      | None -> none_input_value
      | Some value -> string_of_int value
    in
    Eliom_shared.React.S.create initial_value
  in
  let input, value =
    let input, (value, _) =
      reactive_input
        ~validate:[%client fun x -> Result.is_ok (validate_as_int x)]
        ~input_r
        ~a:
          [ a_input_type `Text
          ; a_inputmode `Numeric
          ; a_size size
          ; a_onfocus [%client select_input_value]
          ; a_class ["ot-form-input"] ]
        ()
    in
    input, Eliom_shared.React.S.map [%shared validate_as_int] value
  in
  let less_button, more_button =
    let step_button =
      int_step_button ~min_value:min ~max_value:max ~value
        ~set_value:(snd input_r) ~input_elt:input ~optional
    in
    step_button (-1), step_button 1
  in
  ( F.div ~a:[F.a_class ["ot-form-int-input"]] [less_button; input; more_button]
  , value )

let%shared
    optional_int_input ?(min = 0) ?(max = max_int) ?(size = 2) initial_value
  =
  make_int_input ~min ~max ~size ~optional:true initial_value

let%shared int_input ?(min = 0) ?(max = max_int) ?(size = 2) initial_value =
  let buttons, value =
    make_int_input ~min ~max ~size ~optional:false (Some initial_value)
  in
  let value =
    Eliom_shared.React.S.map
      [%shared
        fun result ->
          match result with
          | Ok (Some s) -> Ok s
          | Ok None | Error () -> Error ()]
      value
  in
  buttons, value

(* ================================================================ *)
(* Tab cycling (client-only)                                        *)
(* ================================================================ *)

module%client Tabbable = struct
  class type t = object
    inherit Dom_html.element
    method tabIndex : int Js.prop
  end
end

let%client only_if_active' elt v =
  if Ot_style.invisible elt then None else Some v

let%client only_if_active elt v =
  if elt##.disabled = Js._true || Ot_style.invisible elt then None else Some v

let%client coerce_to_tabbable x =
  let x = Dom_html.element x in
  match Dom_html.tagged x with
  | Dom_html.A x -> only_if_active' x (x :> Tabbable.t Js.t)
  | Dom_html.Button x -> only_if_active x (x :> Tabbable.t Js.t)
  | Dom_html.Input x -> only_if_active x (x :> Tabbable.t Js.t)
  | Dom_html.Select x -> only_if_active x (x :> Tabbable.t Js.t)
  | Dom_html.Textarea x -> only_if_active x (x :> Tabbable.t Js.t)
  | _ -> None

let%client tabbable_elts_of elt =
  elt##querySelectorAll
    (Js.string
       "a[href],link[href],button,input:not([type=\"hidden\"]),select,textarea,[ot-form-focusable]")
  |> Dom.list_of_nodeList
  |> List.map coerce_to_tabbable
  |> List.fold_left (fun a -> function Some x -> x :: a | _ -> a) []
  |> List.rev

let%client setup_tabcycle (elts : #Tabbable.t Js.t list) : unit =
  let rec fn n = function
    | [x] ->
        x##.tabIndex := n;
        (let open Lwt_js_events in
         async @@ fun () ->
         focuses x @@ fun _ _ ->
         x##.tabIndex := 1;
         Lwt.return_unit);
        let open Lwt_js_events in
        async @@ fun () ->
        blurs x @@ fun _ _ ->
        x##.tabIndex := n;
        Lwt.return_unit
    | hd :: tl ->
        hd##.tabIndex := n;
        fn (n + 1) tl
    | [] -> ()
  in
  fn 2 elts

let%client setup_tabcycle_auto x = setup_tabcycle (tabbable_elts_of x)

let%client focus_first = function
  | x :: _ -> (Js.Unsafe.coerce x)##focus
  | [] -> ()

let%client prevent_tab elt =
  let save_and_set_tabindex idx elt =
    let old = elt##.tabIndex in
    elt##.tabIndex := idx;
    elt, old
  in
  let restore_tabindex (elt, i) = elt##.tabIndex := i in
  let elts = List.map (save_and_set_tabindex (-1)) (tabbable_elts_of elt) in
  fun () -> List.iter restore_tabindex elts

let%client setup_form element =
  let elts = tabbable_elts_of element in
  setup_tabcycle elts; focus_first elts

