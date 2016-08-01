[%%client.start]
open Eliom_content.Html
open Html_types


(* This is about the real "position: sticky" *)

let is_position_sticky elt =
  let pos = Js.to_string
      (Dom_html.window##getComputedStyle (To_dom.of_element elt))##.position
  in pos = "-webkit-sticky" || pos = "sticky"

let set_position_sticky elt =
  is_position_sticky elt || begin
    let old_pos = Manip.Css.position elt in
    Manip.SetCss.position elt "-webkit-sticky";
    is_position_sticky elt || begin
      Manip.SetCss.position elt "sticky";
      is_position_sticky elt || (Manip.SetCss.position elt old_pos; false)
    end
  end

let supports_position_sticky elt =
  let old_pos = Manip.Css.position elt in
  let res = set_position_sticky elt in
  Manip.SetCss.position elt old_pos;
  res

(* This is about the "position: sticky" polyfill *)

let is_sticky elt = is_position_sticky elt
                    || Manip.Class.contain elt "ot-sticky-inline"
                    || Manip.Class.contain elt "ot-sticky-fixed"

type glue = {
  fixed : div_content D.elt;
  inline : div_content D.elt;
  dir : [`Top | `Left]; (*TODO: support `Bottom and `Right*)
  scroll_thread : unit Lwt.t;
  resize_thread : (int * int) React.S.t;
  dissolve : unit -> unit
}

let move_content ~from to_elt =
  if (Ot_style.style @@ To_dom.of_element to_elt)##.display <> Js.string "none" then begin
    let children = Manip.children from in
    Manip.removeChildren from;
    Manip.appendChildren to_elt children
  end

let stick ?(force=false) g =
  if force || not @@ Manip.Class.contain g.fixed "ot-stuck" then begin
    Ot_style.set_width g.inline @@
      Ot_size.client_width (To_dom.of_element g.inline);
    Ot_style.set_height g.inline @@
      Ot_size.client_height (To_dom.of_element g.inline);
    move_content ~from:g.inline g.fixed;
    Manip.Class.add g.fixed "ot-stuck";
    Manip.Class.add g.inline "ot-stuck"
  end

let unstick ?(force=false) g =
  if force || Manip.Class.contain g.fixed "ot-stuck" then begin
    Manip.SetCss.width g.inline "";
    Manip.SetCss.height g.inline "";
    move_content ~from:g.fixed g.inline;
    Manip.Class.remove g.fixed "ot-stuck";
    Manip.Class.remove g.inline "ot-stuck"
  end

let synchronise g =
  let sync_values () =
    Ot_style.set_width g.fixed @@
      Ot_size.client_width (To_dom.of_element g.inline);
    Ot_style.set_height g.fixed @@
      Ot_size.client_height (To_dom.of_element g.inline);
    match g.dir with
      | `Top -> Ot_style.set_left g.fixed @@ Ot_size.client_page_left
          (To_dom.of_element g.inline)
      | `Left -> Ot_style.set_top g.fixed @@ Ot_size.client_page_top
          (To_dom.of_element g.inline)
  in

  if Manip.Class.contain g.fixed "ot-stuck" then begin
    unstick g;
    sync_values ();
    stick g
  end
  else sync_values ()

let update_state ?force g =
  let fixed = To_dom.of_element g.fixed in
  let inline = To_dom.of_element g.inline in
  match g.dir with
  | `Top -> if Ot_size.client_top fixed > Ot_size.client_top inline
              then stick ?force g else unstick ?force g
  | `Left -> if Ot_size.client_left fixed > Ot_size.client_left inline
               then stick ?force g else unstick ?force g

let make_sticky
    ~dir (* TODO: detect based on CSS attribute? *)
    (*TODO: `Bottom and `Right *)
    ?(ios_html_scroll_hack = false)
    elt =

  let%lwt () = Ot_nodeready.nodeready (To_dom.of_element elt) in

  if supports_position_sticky elt then Lwt.return None else begin
    let fixed_dom = Js.Opt.case
        (Dom.CoerceTo.element @@ (To_dom.of_element elt)##cloneNode Js._false)
        (fun () -> failwith "could not clone element to make it sticky")
      (fun x -> x)
    in
    let fixed = Of_dom.of_element @@ Dom_html.element fixed_dom in
    Manip.insertBefore ~before:elt fixed;
    let%lwt () = Ot_nodeready.nodeready fixed_dom in
    Manip.Class.add fixed "ot-sticky-fixed";
    Manip.Class.add elt "ot-sticky-inline";
    let glue = {
      fixed = fixed;
      inline = elt;
      dir = dir;
      scroll_thread = Lwt.return (); (* updated below *)
      resize_thread = React.S.const (0,0); (* updated below *)
      dissolve = fun () -> failwith "undefined"
    } in
    let init () = unstick ~force:true glue; synchronise glue; update_state glue in
    init ();
    let onloaded_thread = Ot_spinner.onloaded |> React.E.map init in
    let scroll_thread = Ot_lib.window_scrolls ~ios_html_scroll_hack @@ fun _ _ ->
      update_state glue; Lwt.return ()
    in
    let resize_thread = Ot_size.width_height |> React.S.map @@
      fun (width, height) -> synchronise glue; update_state glue; (width, height)
    in
    let dissolve () =
      Lwt.cancel scroll_thread;
      React.S.stop resize_thread;
      React.E.stop onloaded_thread;
      unstick ~force:true glue;
      Manip.removeSelf glue.fixed;
      Manip.Class.remove glue.inline "ot-sticky-inline"
    in
    Eliom_client.onunload (fun () -> dissolve ());
    Lwt.return @@ Some {glue with scroll_thread = scroll_thread;
                                  resize_thread = resize_thread;
                                  dissolve = dissolve}
  end

(* This is about functionality built on top of position:sticky / the polyfill *)

let keep_in_sight ~dir ?ios_html_scroll_hack elt =
  let%lwt () = Ot_nodeready.nodeready (To_dom.of_element elt) in
  let%lwt glue = make_sticky ?ios_html_scroll_hack ~dir elt in
  let elt = match glue with | None -> elt | Some g -> g.fixed in
  match Manip.parentNode elt with | None -> Lwt.return (fun () -> ()) | Some parent ->
  let%lwt () = Ot_nodeready.nodeready (To_dom.of_element parent) in
  let compute_top_left (_, win_height) = match dir with
    | `Top ->
      (* sleep, as this should run after make_sticky's handlers *)
      let win_height = float_of_int win_height in
      let parent_top = Ot_size.client_page_top (To_dom.of_element parent) in
      let elt_height = Ot_size.client_height (To_dom.of_element elt) in
      if elt_height > win_height -. parent_top
      then Ot_style.set_top elt (win_height -. elt_height)
      else Ot_style.set_top elt parent_top
    | _ -> failwith "Ot_sticky.keep_in_sight only supports ~dir:`Top right now."
  in
  let resize_thread = React.S.map compute_top_left @@ match glue with
    | None -> Ot_size.width_height
    | Some glue -> glue.resize_thread
  in
  let init () =
    let doIt () = compute_top_left @@ React.S.value Ot_size.width_height in
    (* the additional initialisation after some delay is due to the inexplicable
       behaviour on Chrome where the initialisation happens too early. *)
    Lwt.async (fun () -> let%lwt _ = Lwt_js.sleep 0.5 in Lwt.return @@ doIt ());
    doIt ()
  in
  init ();
  let onload_thread = React.E.map init Ot_spinner.onloaded in
  let stop () =
    React.E.stop onload_thread;
    React.S.stop resize_thread;
    match glue with | Some g -> g.dissolve () | None -> ()
  in
  Eliom_client.onunload (fun () -> stop ());
  Lwt.return stop
