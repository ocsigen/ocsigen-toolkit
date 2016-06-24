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

let is_sticky elt = is_position_sticky elt || Manip.Class.contain elt "ot-sticky"

(*TODO: Everything with *Px is rounded to pixels. We don't want this*)

type set_size = [`Fix | `Sync | `Leave]

type glue = {
  elt : div_content D.elt;
  placeholder : div_content D.elt;
  dir : [`Top | `Left]; (*TODO: support `Bottom and `Right*)
  pos: set_size;
  placeholder_width: set_size;
  placeholder_height: set_size;
  elt_width: set_size;
  elt_height: set_size;
  scroll_thread: unit Lwt.t;
  resize_thread: unit Lwt.t;
}

let synchronise g = if Manip.Class.contain g.elt "ot-stuck" then begin
  if g.placeholder_width = `Sync then Ot_style.set_width g.placeholder @@
    Ot_size.client_width (To_dom.of_element g.elt);
  if g.placeholder_height = `Sync then Ot_style.set_height g.placeholder @@
    Ot_size.client_height (To_dom.of_element g.elt);
  if g.elt_width = `Sync then Ot_style.set_width g.elt @@
    Ot_size.client_width (To_dom.of_element g.placeholder);
  if g.elt_height = `Sync then Ot_style.set_height g.elt @@
    Ot_size.client_height (To_dom.of_element g.placeholder);
  if g.pos = `Sync then begin match g.dir with
    | `Top -> Ot_style.set_left g.elt @@ Ot_size.client_page_left
        (To_dom.of_element g.placeholder)
    | `Left -> Ot_style.set_top g.elt @@ Ot_size.client_page_top
        (To_dom.of_element g.placeholder)
  end
end

let stick g = if not @@ Manip.Class.contain g.elt "ot-stuck" then begin
  (*TODO: save/restore old top/position/height/width values*)
  let elt = To_dom.of_element g.elt in
  let width  = Ot_size.client_width  elt in
  let height = Ot_size.client_height elt in
  if g.placeholder_width  = `Fix then Ot_style.set_width  g.placeholder width;
  if g.placeholder_height = `Fix then Ot_style.set_height g.placeholder height;
  if g.elt_width  = `Fix then Ot_style.set_width  g.elt width;
  if g.elt_height = `Fix then Ot_style.set_height g.elt height;
  if g.pos = `Fix then begin match g.dir with
    | `Top -> Ot_style.set_left g.elt @@ Ot_size.client_page_left elt
    | `Left -> Ot_style.set_top g.elt @@ Ot_size.client_page_top  elt
  end;
  if g.pos = `Fix then begin match g.dir with
    | `Top -> Ot_style.set_left g.elt @@ Ot_size.client_page_left elt
    | `Left -> Ot_style.set_top g.elt @@ Ot_size.client_page_top  elt
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
  match g.dir with
  | `Top ->
    begin match Ot_style.top @@ To_dom.of_element g.elt with
      | None -> detach g
      | Some top -> if Ot_size.client_top pos_elt < top then stick g else detach g
    end
  | `Left ->
    begin match Ot_style.left @@ To_dom.of_element g.elt with
      | None -> detach g
      | Some left -> if Ot_size.client_left pos_elt < left then stick g else detach g
    end

let make_sticky
    ~dir (* TODO: detect based on CSS attribute? *)
    (*TODO: `Bottom and `Right *)
    ?(placeholder_width = if dir = `Top then `Leave else `Sync)
    ?(placeholder_height = if dir = `Top then `Sync else `Leave)
    ?(elt_width = if dir = `Top then `Sync else `Leave)
    ?(elt_height = if dir = `Top then `Leave else `Sync)
    ?(pos = `Sync)
    ?(ios_html_scroll_hack = false)
    elt = if supports_position_sticky elt then None else
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
    scroll_thread = Lwt.return ();
    resize_thread = Lwt.return ();
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
  Some {glue with scroll_thread = scroll_thread; resize_thread = resize_thread}

let dissolve g =
  Lwt.cancel g.scroll_thread;
  Lwt.cancel g.resize_thread;
  detach g;
  Manip.Class.remove g.elt "ot-sticky"

(* This is about functionality built on top of position:sticky / the polyfill *)

type leash = {thread: unit Lwt.t; glue: glue option}

(** Make sure a sticky element never scrolls out of view by dynamically
    modifying the CSS attribute "top" *)
    (* TODO: doc: kill the thread to cancel the effects *)
let keep_in_sight ~dir elt =
  let glue = make_sticky ~dir elt in
  if is_sticky elt
  then begin
    let sight_thread = match dir with
    | `Top -> begin
      let%lwt () = Ot_nodeready.nodeready (To_dom.of_element elt) in
      match Manip.parentNode elt with | None -> Lwt.return () | Some parent ->
      let%lwt () = Ot_nodeready.nodeready (To_dom.of_element parent) in
      let compute_top win_height =
        let win_height = float_of_int win_height in
        let parent_top = Ot_size.client_page_top (To_dom.of_element parent) in
        let elt_height = Ot_size.client_height (To_dom.of_element elt) in
        if elt_height > win_height -. parent_top
          then Ot_style.set_top elt (win_height -. elt_height)
          else Ot_style.set_top elt parent_top
      in
      ignore @@ React.S.map compute_top Ot_size.height;
      ignore @@ React.E.map
        (fun () -> compute_top @@ React.S.value Ot_size.height)
        Ot_spinner.onloaded;
      Lwt.return ()
    end
    | _ -> failwith "Ot_sticky.keep_in_sight only supports ~dir:`Top right now."
    in Some {thread = sight_thread; glue = glue}
  end
  else None

let release leash =
  Lwt.cancel leash.thread;
  match leash.glue with | None -> () | Some glue -> dissolve glue
