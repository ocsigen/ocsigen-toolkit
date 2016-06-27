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

let is_sticky elt = is_position_sticky elt || Manip.Class.contain elt "ot-sticky-fixed"

(*TODO: Everything with *Px is rounded to pixels. We don't want this*)

type set_size = [`Fix | `Sync | `Leave]

type glue = {
  fixed : div_content D.elt;
  inline : div_content D.elt;
  dir : [`Top | `Left]; (*TODO: support `Bottom and `Right*)
  pos: set_size;
  inline_width: set_size;
  inline_height: set_size;
  fixed_width: set_size;
  fixed_height: set_size;
  scroll_thread: unit Lwt.t;
  resize_thread: unit Lwt.t;
}

let synchronise g =
  if g.inline_width = `Sync then Ot_style.set_width g.inline @@
    Ot_size.client_width (To_dom.of_element g.fixed);
  if g.inline_height = `Sync then Ot_style.set_height g.inline @@
    Ot_size.client_height (To_dom.of_element g.fixed);
  if g.fixed_width = `Sync then Ot_style.set_width g.fixed @@
    Ot_size.client_width (To_dom.of_element g.inline);
  if g.fixed_height = `Sync then Ot_style.set_height g.fixed @@
    Ot_size.client_height (To_dom.of_element g.inline);
  if g.pos = `Sync then match g.dir with
    | `Top -> Ot_style.set_left g.fixed @@ Ot_size.client_page_left
        (To_dom.of_element g.inline)
    | `Left -> Ot_style.set_top g.fixed @@ Ot_size.client_page_top
        (To_dom.of_element g.inline)

let fix_size_pos g =
  let fixed = To_dom.of_element g.fixed in
  let width  = Ot_size.client_width  fixed in
  let height = Ot_size.client_height fixed in
  if g.inline_width  = `Fix then Ot_style.set_width  g.inline width;
  if g.inline_height = `Fix then Ot_style.set_height g.inline height;
  if g.fixed_width  = `Fix then Ot_style.set_width  g.fixed width;
  if g.fixed_height = `Fix then Ot_style.set_height g.fixed height;
  if g.pos = `Fix then begin match g.dir with
    | `Top -> Ot_style.set_left g.fixed @@ Ot_size.client_page_left fixed
    | `Left -> Ot_style.set_top g.fixed @@ Ot_size.client_page_top  fixed
  end;
  if g.pos = `Fix then begin match g.dir with
    | `Top -> Ot_style.set_left g.fixed @@ Ot_size.client_page_left fixed
    | `Left -> Ot_style.set_top g.fixed @@ Ot_size.client_page_top  fixed
  end

let stick g = if not @@ Manip.Class.contain g.fixed "ot-stuck" then begin
  Manip.Class.add g.fixed "ot-stuck";
  Manip.Class.add g.inline "ot-stuck"
end

let detach g = if Manip.Class.contain g.fixed "ot-stuck" then begin
  Manip.Class.remove g.fixed "ot-stuck";
  Manip.Class.remove g.inline "ot-stuck"
end

let update_state g =
  let fixed = To_dom.of_element g.fixed in
  let inline = To_dom.of_element g.inline in
  match g.dir with
  | `Top ->
    print_endline @@ string_of_float (Ot_size.client_top fixed) ^ " " ^ string_of_float (Ot_size.client_top inline);
    if Ot_size.client_top fixed > Ot_size.client_top inline
              then stick g else detach g
  | `Left -> if Ot_size.client_left fixed > Ot_size.client_left inline
               then stick g else detach g

(*TODO: doc: should be a D element*)
let make_sticky
    ~dir (* TODO: detect based on CSS attribute? *)
    (*TODO: `Bottom and `Right *)
    ?(inline_width = `Leave)
    ?(inline_height = `Leave)
    ?(fixed_width = `Sync)
    ?(fixed_height = `Sync)
    ?(pos = `Sync)
    ?(ios_html_scroll_hack = false)
    elt = if supports_position_sticky elt then None else
  let inline = Js.Opt.case
      (Dom.CoerceTo.element @@ (To_dom.of_element elt)##cloneNode Js._true)
      (fun () -> failwith "could not clone element to make it sticky")
    (fun x -> x)
  in
  let inline = Of_dom.of_element @@ Dom_html.element inline in
  Manip.insertAfter ~after:elt inline;
  Manip.Class.add elt "ot-sticky-fixed";
  Manip.Class.add inline "ot-sticky-inline";
  let glue = {
    fixed = elt;
    inline = inline;
    dir = dir;
    inline_width = inline_width;
    inline_height = inline_height;
    fixed_width = fixed_width;
    fixed_height = fixed_height;
    scroll_thread = Lwt.return ();
    resize_thread = Lwt.return ();
    pos = pos
  } in
  Lwt.async (fun () ->
    let%lwt () = Ot_nodeready.nodeready @@ To_dom.of_element glue.inline in
    fix_size_pos glue;
    synchronise glue;
    update_state glue;
    Lwt.return ()
  );
  let scroll_thread = begin Ot_lib.window_scrolls ~ios_html_scroll_hack @@ fun _ _ ->
    update_state glue;
    Lwt.return ()
  end in
  let resize_thread = if List.exists (fun x -> x = `Sync)
             [inline_width; inline_height; fixed_width; fixed_height; pos]
    then Ot_lib.onresizes @@ fun _ _ ->
      synchronise glue;
      update_state glue;
      Lwt.return ()
    else Lwt.return ()
  in
  Some {glue with scroll_thread = scroll_thread; resize_thread = resize_thread}

let dissolve g =
  Lwt.cancel g.scroll_thread;
  Lwt.cancel g.resize_thread;
  detach g;
  Manip.removeSelf g.inline;
  Manip.Class.remove g.fixed "ot-sticky-fixed"

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
