(* Ocsigen
 * http://www.ocsigen.org
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

[%%shared open Eliom_content.Html ]
[%%shared open Eliom_content.Html.F ]

[%%shared
type 'a service =
  (unit
  , 'a * ((float * float * float * float) option * Eliom_lib.file_info)
  , Eliom_service.post
  , Eliom_service.non_att
  , Eliom_service.co
  , Eliom_service.non_ext
  , Eliom_service.reg
  , [ `WithoutSuffix ]
  , unit
  , [ `One of 'a Eliom_parameter.ocaml ] Eliom_parameter.param_name
    * ([ `One of (float * float * float * float)
             option Eliom_parameter.ocaml
       ] Eliom_parameter.param_name
       * [ `One of Eliom_lib.file_info ] Eliom_parameter.param_name)
  , unit Eliom_service.ocaml) Eliom_service.t
]

let%client process_file input callback =
  Js.Optdef.case
    (input##.files) (fun () -> Lwt.return ())
    (function files -> Js.Opt.case (files##(item (0))) (fun () -> Lwt.return ())
                         (fun x -> callback x))

let%client file_reader file callback =
  let reader = new%js File.fileReader in
  let () = reader##.onload := Dom.handler (fun e ->
    Js.Opt.case
      (e##.target)
      (fun () -> Js.bool false)
      (fun target ->
         let result = File.CoerceTo.string target##.result in
         Js.Opt.case
           (result)
           (fun () -> Js.bool false)
           (fun blob -> callback blob
                      ; Js.bool false) ) ) in
  let () = reader##readAsDataURL file in ()

let%shared cropper
    ~(image : Dom_html.element Js.t Eliom_client_value.t )
    ?(ratio : float option) () =
  let mk_controller style =
    D.div ~a:[ a_class [ "ot-pup-ctrl" ; "ot-pup-ctrl-" ^ style ] ] [] in
  let t_c = mk_controller "t" in
  let tr_c = mk_controller "tr" in
  let r_c = mk_controller "r" in
  let br_c = mk_controller "br" in
  let b_c = mk_controller "b" in
  let bl_c = mk_controller "bl" in
  let l_c = mk_controller "l" in
  let tl_c = mk_controller "tl" in
  let mk_filter style =
    D.div ~a:[ a_class [ "ot-pup-fltr" ; "ot-pup-fltr-" ^ style ] ] [] in
  let t_f = mk_filter "t" in
  let tr_f = mk_filter "tr" in
  let r_f = mk_filter "r" in
  let br_f = mk_filter "br" in
  let b_f = mk_filter "b" in
  let bl_f = mk_filter "bl" in
  let l_f = mk_filter "l" in
  let tl_f = mk_filter "tl" in
  let crop = D.div ~a:[ a_class [ "ot-pup-ctrls" ] ]
      [ t_c ; tr_c ; r_c ; br_c ; b_c ; bl_c ; l_c ; tl_c ] in
  let left, set_left = Eliom_shared.React.S.create 0. in
  let right, set_right = Eliom_shared.React.S.create 0. in
  let top, set_top = Eliom_shared.React.S.create 0. in
  let bottom, set_bottom = Eliom_shared.React.S.create 0. in
  let img_w, set_img_w = Eliom_shared.React.S.create 0. in
  let img_h, set_img_h = Eliom_shared.React.S.create 0. in
  let _ = [%client
    (let open Lwt_js_events in
     let crop = To_dom.of_element ~%crop in
     let t_f = To_dom.of_element ~%t_f in
     let tr_f = To_dom.of_element ~%tr_f in
     let r_f = To_dom.of_element ~%r_f in
     let br_f = To_dom.of_element ~%br_f in
     let b_f = To_dom.of_element ~%b_f in
     let bl_f = To_dom.of_element ~%bl_f in
     let l_f = To_dom.of_element ~%l_f in
     let tl_f = To_dom.of_element ~%tl_f in
     let x = ref 0. in
     let y = ref 0. in
     ignore @@ React.S.map (fun x ->
       let top = Js.string (Printf.sprintf "%g%%" x) in
       let () = t_f##.style##.height := top in
       let () = tr_f##.style##.height := top in
       let () = tl_f##.style##.height := top in
       let () = l_f##.style##.top := top in
       let () = r_f##.style##.top := top in
       crop##.style##.top := top
     ) ~%top ;
     ignore @@ React.S.map (fun x ->
       let bottom = Js.string (Printf.sprintf "%g%%" x) in
       let () = b_f##.style##.height := bottom in
       let () = br_f##.style##.height := bottom in
       let () = bl_f##.style##.height := bottom in
       let () = l_f##.style##.bottom := bottom in
       let () = r_f##.style##.bottom := bottom in
       crop##.style##.bottom := bottom ) ~%bottom ;
     ignore @@ React.S.map (fun x ->
       let right = Js.string (Printf.sprintf "%g%%" x) in
       let () = r_f##.style##.width := right in
       let () = tr_f##.style##.width := right in
       let () = br_f##.style##.width := right in
       let () = t_f##.style##.right := right in
       let () = b_f##.style##.right := right in
       crop##.style##.right := right) ~%right ;
     ignore @@ React.S.map (fun x ->
       let left = Js.string (Printf.sprintf "%g%%" x) in
       let () = l_f##.style##.width := left in
       let () = tl_f##.style##.width := left in
       let () = bl_f##.style##.width := left in
       let () = t_f##.style##.left := left in
       let () = b_f##.style##.left := left in
       crop##.style##.left := left) ~%left ;
     let update_top dy =
       ~%set_top @@
       min (100. -. React.S.value ~%bottom) @@
       max (React.S.value ~%top +. dy) 0. in
     let update_bottom dy =
       ~%set_bottom @@
       min (100. -. React.S.value ~%top) @@
       max (React.S.value ~%bottom -. dy) 0. in
     let update_right dx =
       ~%set_right @@
       min (100. -. React.S.value ~%left) @@
       max (React.S.value ~%right -. dx) 0. in
     let update_left dx =
       ~%set_left @@
       min (100. -. React.S.value ~%right) @@
       max (React.S.value ~%left +. dx) 0. in
     let plus x : float = +. x in
     let minus x : float = -. x in
     let move_listener = fun cx cy ->
       let dx = (cx -. !x) /. React.S.value ~%img_w in
       let dy = (cy -. !y) /. React.S.value ~%img_h in
       let dx = if dx > 0.
         then min (React.S.value ~%right) dx
         else max (-. (React.S.value ~%left)) dx in
       let dy = if dy > 0.
         then min (React.S.value ~%bottom) dy
         else max (-. (React.S.value ~%top)) dy in
       update_right dx ;
       update_left dx ;
       update_top dy ;
       update_bottom dy ;
       x := cx ;
       y := cy ;
       Js.bool true in
     let mk_listener_ratio ?x_axis ?y_axis~dir ~ratio () cx cy =
       let cx = match x_axis with None -> !x | _ -> cx in
       let cy = match y_axis with None -> !y | _ -> cy in
       let dx = (match x_axis with Some f -> f (cx -. !x)
                                 | None -> cx -. !x)
                /. React.S.value ~%img_w in
       let dy = (match y_axis with Some f -> f (cy -. !y)
                                 | None -> cy -. !y)
                /. React.S.value ~%img_h in
       let dx = if abs_float dx > abs_float dy then dx else dy in
       let dy = dx *. ratio
                *. (React.S.value ~%img_w /. React.S.value ~%img_h) in
       let (t, r, b, l) = match dir with
         | `T  -> ( 1., 0.5,  0., 0.5)
         | `TR -> ( 1.,  1.,  0.,  0.)
         | `R  -> (0.5,  1., 0.5,  0.)
         | `BR -> ( 0.,  1.,  1.,  0.)
         | `B  -> ( 0., 0.5,  1., 0.5)
         | `BL -> ( 0.,  0.,  1.,  1.)
         | `L  -> (0.5,  0., 0.5,  1.)
         | `TL -> ( 1.,  0.,  0.,  1.) in
       if React.S.value ~%right >= r *. dx
       && React.S.value ~%left >= l *. dx
       && React.S.value ~%top >= t *. dy
       && React.S.value ~%bottom >= b *. dy
       then begin
         update_top (-.t *. dy) ;
         update_bottom (b *. dy) ;
         update_left (-.l *. dx) ;
         update_right (r *. dx) end ;
       x := cx ;
       y := cy ;
       Js.bool true in
     let mk_listener = fun ?move_x ?move_y () cx cy ->
       let (cx, move_x) = match move_x with
         | Some f -> (cx, f)
         | None   -> (!x, fun _ -> ()) in
       let (cy, move_y) = match move_y with
         | Some f -> (cy, f)
         | None   -> (!y, fun _ -> ()) in
       let dx = (cx -. !x) /. React.S.value ~%img_w in
       let dy = (cy -. !y) /. React.S.value ~%img_h in
       if dx <> 0. then begin move_x dx ; x := cx end ;
       if dy <> 0. then begin move_y dy ; y := cy end ;
       Js.bool true in
     let bind_handler
       = fun add_trigger event rm_trigger get_x get_y (dom, handler) ->
         Lwt.async (fun () -> add_trigger (To_dom.of_element dom) (fun ev _ ->
           Dom.preventDefault ev ;
           Dom_html.stopPropagation ev ;
           let () = x := get_x ev in
           let () = y := get_y ev in
           let x = Dom_html.addEventListener
               Dom_html.document
               event
               (Dom_html.handler (fun ev -> handler (get_x ev) (get_y ev)) )
               (Js.bool false) in
           let%lwt _ =
             Lwt.pick @@
             List.map (fun e -> e Dom_html.document) rm_trigger in
           Dom_html.removeEventListener x ;
           Lwt.return () ) )
     in
     let listeners = match ~%ratio with
       | Some ratio ->
         [ move_listener
         ; mk_listener_ratio ~dir:`T ~y_axis:minus ~ratio ()
         ; mk_listener_ratio ~dir:`TR ~x_axis:plus ~y_axis:minus ~ratio ()
         ; mk_listener_ratio ~dir:`R ~x_axis:plus ~ratio ()
         ; mk_listener_ratio ~dir:`BR ~x_axis:plus ~y_axis:plus ~ratio ()
         ; mk_listener_ratio ~dir:`B ~y_axis:plus ~ratio ()
         ; mk_listener_ratio ~dir:`BL ~x_axis:minus ~y_axis:plus ~ratio ()
         ; mk_listener_ratio ~dir:`L ~x_axis:minus ~ratio ()
         ; mk_listener_ratio ~dir:`TL ~x_axis:minus ~y_axis:minus ~ratio () ]
       | None ->
         [ move_listener
         ; mk_listener ~move_y:update_top ()
         ; mk_listener ~move_x:update_right ~move_y:update_top ()
         ; mk_listener ~move_x:update_right ()
         ; mk_listener ~move_x:update_right ~move_y:update_bottom ()
         ; mk_listener ~move_y:update_bottom ()
         ; mk_listener ~move_x:update_left ~move_y:update_bottom ()
         ; mk_listener ~move_x:update_left ()
         ; mk_listener ~move_x:update_left ~move_y:update_top () ] in
     List.iter2 (fun x y ->
       bind_handler
         mousedowns
         Dom_html.Event.mousemove
         [Lwt_js_events.mouseup]
         (fun ev -> float_of_int ev##.clientX )
         (fun ev -> float_of_int ev##.clientY )
         (x, y) )
       [ ~%crop ; ~%t_c ; ~%tr_c ; ~%r_c ; ~%br_c
       ; ~%b_c ; ~%bl_c ; ~%l_c ; ~%tl_c ]
       listeners ;
     List.iter2 (fun x y ->
       List.iter (fun x ->
         bind_handler
           touchstarts
           Dom_html.Event.touchmove
           [Lwt_js_events.touchend; Lwt_js_events.touchcancel]
           (fun ev -> float_of_int @@
             Js.Optdef.case
               (ev##.touches##item 0)
               (fun () -> assert false)
               (fun x -> x##.clientX) )
           (fun ev -> float_of_int @@
             Js.Optdef.case
               (ev##.touches##item 0)
               (fun () -> assert false)
               (fun x -> x##.clientY) )
           (x, y) ) x)
       [ [~%crop]
       ; [~%t_f;~%t_c]
       ; [~%tr_f;~%tr_c]
       ; [~%r_f;~%r_c]
       ; [~%br_f;~%br_c]
       ; [~%b_f;~%b_c]
       ; [~%bl_f;~%bl_c]
       ; [~%l_f;~%l_c]
       ; [~%tl_f;~%tl_c] ]
       listeners
     : unit) ] in
  let reset  = [%client (fun () ->
    let bb = ~%image##getBoundingClientRect in
    let bb_w = (bb##.right -. bb##.left) in
    let bb_h = (bb##.bottom -. bb##.top) in
    let (w, h) = match ~%ratio with
      | Some ratio ->
        if ratio *. bb_w <= bb_h then (bb_w, ratio *. bb_w)
        else (bb_h /. ratio, bb_h)
      | None -> (bb_w, bb_h) in
    ~%set_img_w (bb_w /. 100.) ;
    ~%set_img_h (bb_h /. 100.) ;
    ~%set_top 0. ;
    ~%set_right ((bb_w -. w) /. (bb_w /. 100.)) ;
    ~%set_bottom ((bb_h -. h) /. (bb_h /. 100.)) ;
    ~%set_left 0.
    : unit -> unit ) ] in
  ( reset
  , Eliom_shared.React.S.l4
      [%shared fun x y w h -> (x, y, w, h) ] top right bottom left
  , div ~a:[ a_class [ "ot-pup-crop-container" ] ]
      [ t_f ; tr_f ; r_f ; br_f ; b_f ; bl_f ; l_f ; t_f ; tl_f ; crop ] )

let%shared input ?(a = []) content =
  let content = (content :> Html_types.label_content_fun elt list) in
  let input = D.Raw.input ~a:[ a_class ["ot-pup-input"]
                             ; a_input_type `File
                             ; a_accept ["image/*"] ]() in
  (input, label ~a:(a_class ["ot-pup-input-label"] :: a) (input :: content))

let%shared preview ?(a = []) () =
  D.img ~a:(a_class [ "ot-pup-preview"] :: a)
    ~src:(Xml.uri_of_string "")
    ~alt:"" ()

let%shared submit ?(a = []) content =
  D.Raw.button ~a:(a_class ["ot-pup-submit"] :: a)
    content

(* FIXME: To be put in Lwt_js_events *)
let%client loads ?cancel_handler ?use_capture t =
  Lwt_js_events.(seq_loop load ?cancel_handler ?use_capture t)

let%client bind_input input preview ?container ?reset () =
  let onerror () =
    Eliom_lib.Option.iter (fun container ->
      container##.classList##add (Js.string "ot-no-file")) container ;
    preview##.src := Js.string "" ;
    Lwt.return () in
  Eliom_lib.Option.iter (fun f ->
    Lwt.async (fun () -> loads preview (fun _ _ -> Lwt.return @@ f () ) ) )
    reset ;
  Lwt.async (fun () -> Lwt_js_events.changes input (fun _ _ ->
    Js.Optdef.case (input##.files) onerror
      (fun files -> Js.Opt.case (files##item 0) onerror
          (fun file ->
             let () = file_reader
                 (Js.Unsafe.coerce file)
                 (fun data ->
                    preview##.src := data ;
                    Eliom_lib.Option.iter (fun container ->
                      container##.classList##remove (Js.string "ot-no-file") )
                      container ) in
             Lwt.return () ) ) ) )

[%%shared
type cropping = (float * float * float * float) React.S.t

type upload = ?cropping:cropping -> File.file Js.t -> unit Lwt.t
]

let%client ocaml_service_upload ~service ~arg ?cropping file =
  Eliom_client.call_ocaml_service service ()
    (arg, (Eliom_lib.Option.map React.S.value cropping, file) )

let%client do_submit input ?cropping ~upload () =
  process_file input (fun file -> upload ?cropping file)

let%client bind_submit
    (input : Dom_html.inputElement Js.t Eliom_client_value.t)
    button ?cropping ~upload ~after_submit () =
  Lwt.async (fun () -> Lwt_js_events.clicks button (fun ev _ ->
    Dom.preventDefault ev ;
    Dom_html.stopPropagation ev ;
    let%lwt () = do_submit input ?cropping ~upload () in
    after_submit () ) )

let%client bind
    ?container ~input ~preview ?crop ~submit ~upload ~after_submit ()
  =
  let (reset, cropping) = match crop with
    | Some (x,y) -> Some x, Some y
    | _          -> None, None in
  let () = bind_input input preview ?container ?reset () in
  let () = bind_submit
      input submit ?cropping ~upload ~after_submit () in
  ()

let%shared mk_service name arg_deriver =
  Eliom_service.create_ocaml
    ~name
    ~id:Eliom_service.Global
    ~meth:
      (Eliom_service.Post
         (Eliom_parameter.unit,
          Eliom_parameter.(
            ocaml "service_arg" arg_deriver **
            ocaml "cropping"
              [%derive.json: (float * float * float * float) option ]
            ** file "f")))
    ()

let%shared mk_form
    ?(after_submit = fun () -> Lwt.return ())
    ?crop
    ?input:(input_a, input_content = [], [])
    ?submit:(submit_a, submit_content = [], [])
    (upload : ?cropping:cropping -> File.file Js.t -> unit Lwt.t) =
  let preview = preview () in
  let (input, input_label) = input ~a:input_a input_content in
  let submit = submit ~a:submit_a submit_content in
  let (crop, cropper_dom) = match crop with
    | Some ratio ->
      let (reset, cropping, cropper_dom) =
        cropper
          ?ratio
          ~image:[%client To_dom.of_element ~%preview ] () in
      (Some (reset, cropping), [ cropper_dom ])
    | None -> (None, []) in
  let form =
    D.form ~a:[ a_class [ "ot-pup-form" ; "ot-no-file" ] ]
      [ input_label
      ; div ~a:[ a_class [ "ot-pup-container" ] ] ( preview :: cropper_dom)
      ; submit ] in

  let _ = [%client (bind
                      ~container:(To_dom.of_form ~%form)
                      ~input:(To_dom.of_input ~%input)
                      ~preview:(To_dom.of_img ~%preview)
                      ?crop:~%crop
                      ~submit:(To_dom.of_button ~%submit)
                      ~upload:~%upload
                      ~after_submit:~%after_submit
                      () : unit) ] in
  Lwt.return form
