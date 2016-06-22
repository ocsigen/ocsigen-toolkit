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


(* This is about the "position: sticky" polyfill *)

let is_sticky elt = is_position_sticky elt || Manip.Class.contain elt "ot-sticky"

(*TODO: Everything with *Px is rounded to pixels. We don't want this*)

type set_size = [`Fix | `Sync | `Leave]

(*TODO: in the docs: warn of cycles*)
type glue = {
  elt : div_content D.elt;
  placeholder : div_content D.elt;
  dir : [`Top | `Left]; (*TODO: support `Bottom and `Right*)
  pos: set_size;
  placeholder_width: set_size;
  placeholder_height: set_size;
  elt_width: set_size;
  elt_height: set_size;
}

let synchronise g = if Manip.Class.contain g.elt "ot-stuck" then begin
  if g.placeholder_width = `Sync then
    Manip.SetCss.widthPx g.placeholder @@ Manip.Attr.offsetWidth g.elt;
  if g.placeholder_height = `Sync then
    Manip.SetCss.heightPx g.placeholder @@ Manip.Attr.offsetHeight g.elt;
  if g.elt_width = `Sync then
    Manip.SetCss.widthPx g.elt @@ Manip.Attr.offsetWidth g.placeholder;
  if g.elt_height = `Sync then
    Manip.SetCss.heightPx g.elt @@ Manip.Attr.offsetHeight g.placeholder;
  if g.pos = `Sync then begin match g.dir with
    | `Top -> Manip.SetCss.leftPx g.elt @@ Ot_size.client_page_left
        (To_dom.of_element g.placeholder)
    | `Left -> Manip.SetCss.topPx g.elt @@ Ot_size.client_page_top
        (To_dom.of_element g.placeholder)
  end
end

let stick g = if not @@ Manip.Class.contain g.elt "ot-stuck" then begin
  (*TODO: save/restore old top/position/height/width values*)
  let height = Manip.Attr.offsetHeight g.elt in
  let width = Manip.Attr.offsetWidth g.elt in
  if g.placeholder_width = `Fix then Manip.SetCss.widthPx g.placeholder width;
  if g.placeholder_height = `Fix then Manip.SetCss.heightPx g.placeholder height;
  if g.elt_width = `Fix then Manip.SetCss.widthPx g.elt width;
  if g.elt_height = `Fix then Manip.SetCss.heightPx g.elt height;
  if g.pos = `Fix then begin match g.dir with
    | `Top -> Manip.SetCss.leftPx g.elt @@ Ot_size.client_page_left
        (To_dom.of_element g.elt)
    | `Left -> Manip.SetCss.topPx g.elt @@ Ot_size.client_page_top
        (To_dom.of_element g.elt)
  end;
  if g.pos = `Fix then begin match g.dir with
    | `Top -> Manip.SetCss.leftPx g.elt @@ Ot_size.client_page_left
        (To_dom.of_element g.elt)
    | `Left -> Manip.SetCss.topPx g.elt @@ Ot_size.client_page_top
        (To_dom.of_element g.elt)
  end;
  Manip.Class.add g.elt "ot-stuck";
  Manip.Class.add g.placeholder "ot-stuck";
  Manip.insertAfter ~after:g.elt g.placeholder;
  synchronise g
end

let detach g = if Manip.Class.contain g.elt "ot-stuck" then begin
  Manip.removeSelf g.placeholder;
  (*TODO: save/restore old top/position/height/width values*)
  Manip.SetCss.height g.elt "";
  Manip.SetCss.width g.elt "";
  begin match g.dir with
    | `Top -> Manip.SetCss.left g.elt ""
    | `Left -> Manip.SetCss.top g.elt ""
  end;
  Manip.Class.remove g.elt "ot-stuck";
  Manip.Class.remove g.placeholder "ot-stuck"
end

let update_state g =
  let pos_elt = To_dom.of_element @@
    if Manip.Class.contain g.elt "ot-stuck" then g.placeholder else g.elt in
  let parse_margin str = match Ot_lib.parse_px str with | None -> 0.0 | Some n -> n in
  let computed_style elt = Dom_html.window##getComputedStyle elt in
  match g.dir with
  | `Top ->
    let marginTop = parse_margin @@ (computed_style pos_elt)##.marginTop in
    let pos = pos_elt##getBoundingClientRect##.top -. marginTop in
    begin match Ot_lib.parse_px @@ (computed_style @@ To_dom.of_element g.elt)##.top with
      | None -> detach g
      | Some top -> if pos < top then stick g else detach g
    end
  | `Left ->
    let marginLeft = parse_margin @@ (computed_style pos_elt)##.marginLeft in
    let pos = pos_elt##getBoundingClientRect##.left -. marginLeft in
    begin match Ot_lib.parse_px @@ (computed_style @@ To_dom.of_element g.elt)##.left with
      | None -> detach g
      | Some left -> if pos < left then stick g else detach g
    end

(*TODO: should have no effect if position:sticky is supported*)
    (* because even if sticky is not set*)
    (*TODO: doc: kill the thread to make it unsticky *)
let make_sticky
    ~dir (* TODO: detect based on CSS attribute? *)
    (*TODO: `Bottom and `Right *)
    ?(placeholder_width = if dir = `Top then `Leave else `Sync)
    ?(placeholder_height = if dir = `Top then `Sync else `Leave)
    ?(elt_width = if dir = `Top then `Sync else `Leave)
    ?(elt_height = if dir = `Top then `Leave else `Sync)
    ?(pos = `Sync)
    ?(ios_html_scroll_hack = false)
    elt = if is_position_sticky elt then Lwt.return () else
  let placeholder = Js.Opt.case
      (Dom.CoerceTo.element @@ (To_dom.of_element elt)##cloneNode Js._false)
    (fun () -> failwith "muh")
    (fun x -> x)
  in
  let placeholder = Of_dom.of_element @@ Dom_html.element placeholder in
  Manip.Class.add elt "ot-sticky";
  Manip.Class.add placeholder "ot-sticky-placeholder";
  let glue = {
    elt = elt;
    placeholder = placeholder;
    dir = dir;
    placeholder_width = placeholder_width;
    placeholder_height = placeholder_height;
    elt_width = elt_width;
    elt_height = elt_height;
    pos = pos
  } in
  let scroll_thread = begin Ot_lib.window_scrolls ~ios_html_scroll_hack @@ fun _ _ ->
    update_state glue;
    Lwt.return ()
  end in
  let resize_thread = if List.exists (fun x -> x = `Sync)
             [placeholder_width; placeholder_height; elt_width; elt_height; pos]
    then Ot_lib.onresizes @@ fun _ _ ->
      update_state glue;
      synchronise glue;
      Lwt.return ()
    else Lwt.return ()
  in
  Lwt.pick [scroll_thread; resize_thread]

(* This is about functionality built on top of position:sticky / the polyfill *)

(** Make sure a sticky element never scrolls out of view by dynamically
    modifying the CSS attribute "top" *)
    (* TODO: doc: kill the thread to cancel the effects *)
let keep_in_sight ~dir elt =
  let sticky_thread = make_sticky ~dir elt in
  let sight_thread = match dir with
  | `Top -> begin
    let%lwt () = Ot_nodeready.nodeready (To_dom.of_element elt) in
    match Manip.parentNode elt with | None -> Lwt.return () | Some parent ->
    let%lwt () = Ot_nodeready.nodeready (To_dom.of_element parent) in
    let compute_top win_height =
      let parent_top = Ot_size.client_page_top (To_dom.of_element parent) in
      let elt_height = Manip.Attr.clientHeight elt in
      if elt_height > win_height - parent_top
        then Manip.SetCss.topPx elt (win_height - elt_height)
        else Manip.SetCss.topPx elt parent_top
    in
    ignore @@ React.S.map compute_top Ot_size.height;
    ignore @@ React.E.map
      (fun () -> compute_top @@ React.S.value Ot_size.height)
      Ot_spinner.onloaded;
    Lwt.return ()
  end
  | _ -> failwith "Ot_sticky.keep_in_sight only supports ~dir:`Top right now."
  in
  Lwt.pick [sticky_thread; sight_thread]
