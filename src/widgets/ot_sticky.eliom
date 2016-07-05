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
  scroll_thread: unit Lwt.t;
  resize_thread: unit Lwt.t;
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
      scroll_thread = Lwt.return ();
      resize_thread = Lwt.return ();
    } in
    Lwt.async (fun () ->
      synchronise glue;
      update_state ~force:true glue;
      Lwt.return ()
    );
    let st = Ot_lib.window_scrolls ~ios_html_scroll_hack @@ fun _ _ ->
      update_state glue;
      Lwt.return ()
    in
    let rt = Ot_lib.onresizes @@ fun _ _ ->
      synchronise glue;
      update_state glue;
      Lwt.return ()
    in
    Lwt.return @@ Some {glue with scroll_thread = st; resize_thread = rt}
  end

let dissolve g =
  Lwt.cancel g.scroll_thread;
  Lwt.cancel g.resize_thread;
  unstick ~force:true g;
  Manip.removeSelf g.fixed;
  Manip.Class.remove g.inline "ot-sticky-inline"

(* This is about functionality built on top of position:sticky / the polyfill *)

type leash = {thread: unit Lwt.t; glue: glue option}

let keep_in_sight ~dir ?ios_html_scroll_hack elt =
  let%lwt () = Ot_nodeready.nodeready (To_dom.of_element elt) in
  let%lwt glue = make_sticky ?ios_html_scroll_hack ~dir elt in
  let elt = match glue with | None -> elt | Some g -> g.fixed in
  let sight_thread = match dir with
  | `Top -> begin
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
    let s = React.S.map compute_top Ot_size.height in
    let e = React.E.map
      (fun () -> compute_top @@ React.S.value Ot_size.height)
      Ot_spinner.onloaded in
    compute_top @@ React.S.value Ot_size.height;
    Eliom_client.onunload
      (fun () ->
         React.S.stop ~strong:true s; React.E.stop ~strong:true e; None);
    Lwt.return ()
  end
  | _ -> failwith "Ot_sticky.keep_in_sight only supports ~dir:`Top right now."
  in Lwt.return {thread = sight_thread; glue = glue}

type leashes = {threads: unit Lwt.t; glues: glue list}

let rec lwt_sequence xs = match xs with
  | [] -> Lwt.return []
  | x::xs ->
    let%lwt x = x in
    let%lwt xs = lwt_sequence xs in
    Lwt.return (x::xs)

let rec list_of_opts = function
  | [] -> []
  | None :: xs -> list_of_opts xs
  | Some x :: xs -> x :: list_of_opts xs

let maximum = function
    [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left max x xs

let minimum = function
    [] -> invalid_arg "empty list"
  | x::xs -> List.fold_left min x xs

let keep_in_sights ~dir ?ios_html_scroll_hack elts =
  let%lwt _ = lwt_sequence @@ List.map
    (fun elt -> Ot_nodeready.nodeready @@ To_dom.of_element elt) elts in
  let%lwt elts_glues = lwt_sequence (elts |> List.map @@ fun elt ->
    let%lwt glue = make_sticky ?ios_html_scroll_hack ~dir elt in
    let elt = match glue with | None -> elt | Some g -> g.fixed in
    Lwt.return (elt, glue)
  ) in
  let elts, glues = List.split elts_glues in
  let glues = list_of_opts glues in
  let sight_thread = match dir with
  | `Top -> begin
    let%lwt elts_parents = lwt_sequence (elts |> List.map @@ fun elt ->
      match Manip.parentNode elt with
      | None -> failwith "Ot_sticky.keep_in_sights: elements need to have parents"
      | Some parent ->
        let parent = To_dom.of_element parent in
        let%lwt () = Ot_nodeready.nodeready parent in
        Lwt.return (elt, parent)
    ) in
    let elts, parents = List.split elts_parents in
    let compute_top win_height =
      let win_height = float_of_int win_height in
      let parent_top = minimum (parents |> List.map @@
        fun parent -> Ot_size.client_page_top parent) in
      let parent_bottom = maximum (parents |> List.map @@
        fun parent -> Ot_size.client_page_bottom parent) in
      let total_height = parent_bottom -. parent_top in
      (*
      let elt_height = Ot_size.client_height (To_dom.of_element elt) in
      *)
      if total_height > win_height -. parent_top
        then ignore (elts_parents |> List.map @@ fun (elt, parent) ->
          Ot_style.set_top elt (win_height -. parent_bottom +. Ot_size.client_page_top parent))
        else ignore (elts_parents |> List.map @@ fun (elt, parent) ->
          Ot_style.set_top elt (Ot_size.client_page_top parent));
      ()
    in
    ignore @@ React.S.map compute_top Ot_size.height;
    ignore @@ React.E.map
      (fun () -> compute_top @@ React.S.value Ot_size.height)
      Ot_spinner.onloaded;
    compute_top @@ React.S.value Ot_size.height;
    Lwt.return ()
  end
  | _ -> failwith "Ot_sticky.keep_in_sight only supports ~dir:`Top right now."
  in Lwt.return {threads = sight_thread; glues = glues}


let release leash =
  Lwt.cancel leash.thread;
  match leash.glue with | None -> () | Some glue -> dissolve glue

let releases leashes =
  Lwt.cancel leashes.threads;
  ignore @@ List.map dissolve leashes.glues
