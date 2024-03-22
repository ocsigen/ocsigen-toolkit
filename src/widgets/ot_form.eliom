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

open%client Js_of_ocaml
open%client Js_of_ocaml_lwt
open%shared Eliom_content.Html
open%shared Eliom_content.Html.F

class type tabbable = object
  inherit Dom_html.element
  method tabIndex : int Js.prop
end

let only_if_active' elt v = if Ot_style.invisible elt then None else Some v

let only_if_active elt v =
  if elt##.disabled = Js._true || Ot_style.invisible elt then None else Some v

let coerce_to_tabbable x =
  let x = Dom_html.element x in
  match Dom_html.tagged x with
  | Dom_html.A x -> only_if_active' x (x :> tabbable Js.t)
  (* | Dom_html.Link     x -> Some (x :> tabbable Js.t) *)
  | Dom_html.Button x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Input x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Select x -> only_if_active x (x :> tabbable Js.t)
  | Dom_html.Textarea x -> only_if_active x (x :> tabbable Js.t)
  (* | Dom_html.Menuitem x -> Some (x :> tabbable Js.t) *)
  | _ -> None

(* https://www.w3.org/TR/html5/editing.html#sequential-focus-navigation-and-the-tabindex-attribute *)
let tabbable_elts_of elt =
  elt##querySelectorAll
    (Js.string
       "a[href],link[href],button,input:not([type=\"hidden\"]),select,textarea,[ot-form-focusable]")
  |> Dom.list_of_nodeList
  |> List.map coerce_to_tabbable
  |> List.fold_left (fun a -> function Some x -> x :: a | _ -> a) []
  |> List.rev

let setup_tabcycle (elts : #tabbable Js.t list) : unit =
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

let setup_tabcycle_auto x = setup_tabcycle (tabbable_elts_of x)
let focus_first = function x :: _ -> (Js.Unsafe.coerce x)##focus | [] -> ()

let prevent_tab elt =
  let save_and_set_tabindex idx elt =
    let old = elt##.tabIndex in
    elt##.tabIndex := idx;
    elt, old
  in
  let restore_tabindex (elt, i) = elt##.tabIndex := i in
  let elts = List.map (save_and_set_tabindex (-1)) (tabbable_elts_of elt) in
  fun () -> List.iter restore_tabindex elts

let setup_form element =
  let elts = tabbable_elts_of element in
  setup_tabcycle elts; focus_first elts

[%%client (* Copyright Vincent Balat - not part of Be Sport *) open Js_of_ocaml]
[%%client open Js_of_ocaml_lwt]
[%%shared open Eliom_content.Html]
[%%shared open Eliom_content.Html.F]

(* Simple button being disabled or not according to a signal *)
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

let%shared radio ?(a = []) ?(disabled_s = Eliom_shared.React.S.const false)
    ?(checked_s = Eliom_shared.React.S.const false) ?name ?(before = []) content
  =
  let a_checked =
    R.filter_attrib (a_checked ())
    @@ Eliom_shared.React.S.l2
         [%shared fun checked disabled -> if disabled then false else checked]
         checked_s disabled_s
  in
  let a_disabled = R.filter_attrib (a_disabled ()) disabled_s in
  let a' = [a_disabled; a_checked] in
  let a' = a_class ["radio-input"] :: a_input_type `Radio :: a' in
  let e =
    D.input ~a:(match name with Some v -> a_name v :: a' | None -> a') ()
  in
  let l =
    D.label
      ~a:(a_class ["radio"] :: (a :> Html_types.label_attrib attrib list))
      (before @ [span ~a:[a_class ["radio-label"]] content; e])
  in
  l, e

let%shared radio_buttons ?(a = [])
    ?(disabled_s =
      (Eliom_shared.React.S.const [] : int list Eliom_shared.React.S.t))
    ~(selection_react : int option Bs_lib.React.component) ~name contents
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
         (* NOTE: checked event is not fired when deselected *)
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
    ~a:(F.a_class ["simple-radio-selector-container"] :: a)
    [ F.div ~a:[F.a_class ["simple-radio-selector-label"]] label
    ; F.div ~a:[F.a_class ["simple-radio-selector"]] buttons ]

let%shared reactive_toggle_button ?(a = []) ?(init = false)
    ?(ctrl = Eliom_shared.React.S.create init) ~check content
  =
  let signal, set_signal = ctrl in
  let onclick =
    [%client
      fun ev ->
        Dom_html.stopPropagation ev;
        Dom.preventDefault ev;
        Lwt.async (fun () ->
          ~%set_signal (not (React.S.value ~%signal));
          Lwt.return_unit)]
  in
  let e =
    Bs_lib.React.node_map
      [%shared
        fun is_checked ->
          let icn, emph = ~%check is_checked in
          Bs_buttons.Emph.btn
            ~a:(a_onclick ~%onclick :: ~%a)
            ~icn ~emph ~%content]
      signal
  in
  (e : [`Button] elt :> [> `Button] elt), (signal, set_signal)

let%client set_validity e b =
  let class_ = Js.string "invalid" in
  if b then e##.classList##remove class_ else e##.classList##add class_

let%client valid e =
  try
    (Js.Unsafe.coerce e)##checkValidity
    && not (Js.to_bool (e##.classList##contains (Js.string "invalid")))
  with _ -> true

let%shared reactify_input ?(input_r : string Bs_lib.React.component option)
    ?(value = "") ?(validate : (string -> bool) Eliom_client_value.t option)
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
  let _ =
    [%client
      (React.S.map
         (fun s ->
            if Js.to_string ~%e_with_value##.value <> s
            then ~%e_with_value##.value := Js.string s)
         ~%signal
       : unit React.S.t)]
  in
  let set_signal =
    [%client
      (let f _ _ =
         let v = Js.to_string ~%e_with_value##.value in
         ~%set_signal v; Lwt.return_unit
       in
       Lwt.async (fun () -> Lwt_js_events.inputs ~%e f);
       (match ~%validate with
       | Some f ->
           ignore (React.S.map (fun x -> set_validity ~%e (f x)) ~%signal)
       | None -> ());
       fun value -> ~%set_signal value
       : _ -> _)]
  in
  signal, set_signal

let%shared reactive_input ?(a = [])
    ?(input_r : string Bs_lib.React.component option) ?value ?validate ()
  =
  let a = (a :> Html_types.input_attrib attrib list) in
  let e =
    D.Raw.input ~a:(match value with Some v -> a_value v :: a | None -> a) ()
  in
  let signal = reactify_input ?input_r ?value ?validate e in
  e, signal

let%shared textarea ?(a = []) ?a_rows:(r = 4) ?(resize = false)
    ?a_placeholder:(p = "") value
  =
  let elt_ref = ref @@ D.Raw.textarea @@ txt value in
  let resize_cb =
    [%client
      fun e ->
        match Dom_html.opt_tagged e##.currentTarget with
        | Some (Dom_html.Textarea elt) -> Bs_lib.resize_textarea elt
        | _ -> Js.Opt.iter e##.currentTarget Bs_lib.resize_textarea]
  in
  let resize_onload =
    [%client
      fun _ ->
        Lwt.async @@ fun () ->
        let%lwt () = Lwt_js_events.request_animation_frame () in
        Bs_lib.resize_textarea @@ To_dom.of_textarea !(~%elt_ref);
        Lwt.return_unit]
  in
  let resize_onload =
    Option.only_if resize @@ fun () -> a_onload resize_onload
  in
  let resize_oninput = Option.only_if resize @@ fun () -> a_oninput resize_cb in
  let a = (a :> Html_types.textarea_attrib Eliom_content.Html.attrib list) in
  let ta =
    D.Raw.textarea
      ~a:
        (a_class ["input"; "form-input"; "form-textarea"]
        @: a_rows r @: a_placeholder p @: resize_oninput @? resize_onload @? a)
      (txt value)
  in
  elt_ref := ta;
  ta

let%shared reactive_textarea ?(a = []) ?a_rows ?resize ?a_placeholder ?value
    ?validate ()
  =
  let a = (a :> Html_types.textarea_attrib attrib list) in
  let e =
    textarea ~a ?a_rows ?resize ?a_placeholder
      (match value with Some v -> v | None -> "")
  in
  let signal = reactify_input ?value ?validate e in
  e, signal

let%client on_enter ~f inp =
  Lwt.async @@ fun () ->
  Lwt_js_events.keydowns inp @@ fun ev _ ->
  if ev##.keyCode = 13 && valid inp
  then f (Js.to_string inp##.value)
  else Lwt.return_unit

let%shared lwt_bind_input_enter
    ?(validate : (string -> bool) Eliom_client_value.t option) ?button
    (e : Html_types.input elt) (f : (string -> unit Lwt.t) Eliom_client_value.t)
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

type%shared checkbox_position = [`Left | `Right | `None]
type%shared checkbox_style = [`Box | `Bullet | `Small_bullet | `Toggle]

let%shared checkbox ?(a = []) ?(a_inp = []) ?(required = false)
    ?(position = (`Right :> checkbox_position)) ?(checked = false)
    ?(readonly = false) ?(disabled = false) ?(style = `Box)
    ?(mobile_style : checkbox_style option) content
  =
  let mobile_style =
    match mobile_style with
    | None -> if style = `Box then `Toggle else style
    | Some s -> s
  in
  let inp =
    D.Raw.input
      ~a:
        ((if readonly || disabled
          then [a_disabled ()]
          else
            []
            (* the `readonly` attribute has no effect on checkboxes and radios.
             * We use `disabled` without the class, instead *))
        @ (if required then [a_required ()] else [])
        @ a_class ["checkbox-input"] :: a_input_type `Checkbox
          :: (if checked then [a_checked ()] else [])
        @ (a_inp :> Html_types.input_attrib attrib list))
      ()
  in
  let box =
    let disabled_class = Option.only_if disabled (fun () -> "disabled") in
    let style_classes_desktop =
      match style with
      | `Small_bullet -> ["checkbox-bullet-desktop"; "small-checkbox-bullet"]
      | `Bullet -> ["checkbox-bullet-desktop"]
      | `Box -> ["checkbox-box-desktop"]
      | `Toggle -> ["checkbox-toggle-desktop"]
    in
    let style_classes_mobile =
      match mobile_style with
      | `Small_bullet -> ["checkbox-bullet-mobile"; "small-checkbox-bullet"]
      | `Bullet -> ["checkbox-bullet-mobile"]
      | `Box -> ["checkbox-box-mobile"]
      | `Toggle -> ["checkbox-toggle-mobile"]
    in
    let style_classes = style_classes_desktop @ style_classes_mobile in
    let position_class =
      match position with
      | `Left -> Some "checkbox-left"
      | `Right -> Some "checkbox-right"
      | `None -> None
    in
    label
      ~a:
        (a_class (disabled_class @? "checkbox" @: style_classes)
        :: (a :> Html_types.label_attrib attrib list))
      [ inp
      ; span
          ~a:[a_class ("checkbox-label" @$? position_class)]
          [ (span
               ~a:[a_class ["checkbox-decoration"]]
               [span ~a:[a_class ["checkbox-sub-decoration"]] []]
              :> [< Html_types.span_content] Eliom_content.Html.elt)
          ; span content ] ]
  in
  box, inp

let%shared reactive_checkbox ?a ?a_inp ?position ?(checked = false) ?readonly
    ?disabled ?style ?mobile_style ?(ctrl = Eliom_shared.React.S.create checked)
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
      let disabled = Option.default false ~%disabled in
      if not disabled
      then
        let inp' = To_dom.of_input ~%inp in
        let _ =
          Eliom_shared.React.S.map
            (fun b -> inp'##.checked := Js.bool b)
            ~%signal
        in
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

let%client input_checker ?(init = false) p ev =
  Option.default init @@ Bs_lib.Target.Input.map p ev

let%client set_custom_validity inp r =
  ignore
  @@
  match r with
  | Ok _ -> (Js.Unsafe.coerce inp)##setCustomValidity (Js.string "")
  | Error s -> (Js.Unsafe.coerce inp)##setCustomValidity (Js.string s)

let%shared input_validation_tools ?init ?set_focus
    ?(result_iter :
       (Js_of_ocaml.Dom_html.inputElement Js_of_ocaml.Js.t -> string -> unit)
         Eliom_client_value.t
         option) ?(invalid_class = "invalid")
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
    @@ Option.map_default
         (fun i -> (Eliom_shared.Value.local check) i)
         (Ok "") init
  in
  let init =
    Option.map
      (fun i -> `Default (Result.is_ok @@ (Eliom_shared.Value.local check) i))
      init
  in
  let a_oninput, valid_s =
    Bs_lib.React.A.check_oninput ?init
      [%client
        input_checker @@ fun t ->
        let r = ~%check (Js.to_string t##.value) in
        ~%set_result r; Result.is_ok r]
  in
  let a_onfocus_o =
    Option.map
      (fun set_focus -> a_onfocus [%client fun _ -> ~%set_focus true])
      set_focus
  in
  let a_onblur, lazy_invalid_s =
    Bs_lib.React.A.check_onblur ?init
      [%client
        input_checker @@ fun t ->
        let r = ~%check (Js.to_string t##.value) in
        let () = Option.may (fun f -> Result.iter (f t) r) ~%result_iter in
        let () = Option.may (fun f -> f false) ~%set_focus in
        ~%set_result r; set_custom_validity t r; Result.is_error r]
  in
  ( a_onfocus_o @? [a_onblur; a_oninput]
  , invalid_cl valid_s lazy_invalid_s
  , result_s )

let%shared graceful_invalid_style
    (inp : Html_types.input Eliom_content.Html.elt)
  =
  ignore
  @@ [%client
       (let inp = Eliom_content.Html.To_dom.of_input ~%inp in
        let f () = set_validity inp (Js.Unsafe.coerce inp)##checkValidity in
        Lwt.async @@ fun () ->
        let%lwt _ = Lwt_js_events.blur inp in
        f ();
        Lwt_js_events.inputs inp @@ fun _ _ -> f (); Lwt.return_unit
        : unit)]

let%shared none_input_value = "-"

let%shared validate_as_int value =
  let value = String.trim value in
  if String.is_empty value || value = none_input_value
  then Ok None
  else
    match int_of_string_opt value with
    | None -> Error ()
    | Some value -> Ok (Some value)

let%shared int_step_button ~min_value ~max_value ~value ~set_value ~icn
    ~optional step
  =
  let disabled =
    let is_disabled =
      [%shared
        function
        | Error () -> true
        | Ok None -> ~%step < 0
        | Ok (Some v) ->
            if ~%step < 0
            then if ~%optional then v < ~%min_value else v <= ~%min_value
            else v >= ~%max_value]
    in
    Eliom_shared.React.S.map is_disabled value
  in
  let execute_step =
    [%client
      fun _ ->
        let value =
          match Eliom_shared.React.S.value ~%value with
          | Ok None | Error () -> pred ~%min_value
          | Ok (Some value) -> value
        in
        if ~%step < 0 && value = ~%min_value && ~%optional
        then ~%set_value none_input_value
        else ~%set_value @@ string_of_int @@ ( + ) ~%step @@ value]
  in
  Bs_buttons.Emph.btn ~icn ~emph:`Med_tertiary
    ~a:[F.a_onclick execute_step; R.filter_attrib (F.a_disabled ()) disabled]
    []

let%client select_input_value ev =
  Js.Opt.iter ev##.currentTarget @@ fun input ->
  match Dom_html.tagged input with
  | Dom_html.Input input -> input##select
  | _ -> ()

(* Auxiliary function for factorization of optional_int_input and
   non_optional_int_input *)
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
        ~validate:[%client Result.is_ok % validate_as_int]
        ~input_r
        ~a:
          [ a_input_type `Text
          ; a_inputmode `Numeric
          ; a_size size
          ; a_onfocus [%client select_input_value]
          ; a_class ["input"] ]
        ()
    in
    input, Eliom_shared.React.S.map [%shared validate_as_int] value
  in
  let less_button, more_button =
    let step_button =
      int_step_button ~min_value:min ~max_value:max ~value
        ~set_value:(snd input_r) ~optional
    in
    step_button ~icn:Bs_icons.minus (-1), step_button ~icn:Bs_icons.plus 1
  in
  ( F.div ~a:[F.a_class ["form-int-input"]] [less_button; input; more_button]
  , value )

let%shared optional_int_input ?(min = 0) ?(max = max_int) ?(size = 2)
    initial_value
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
