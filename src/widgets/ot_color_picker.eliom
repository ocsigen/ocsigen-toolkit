[%%shared

open Eliom_content.Html
open Eliom_content.Html.F

type div = Html_types.div Eliom_content.Html.D.elt
type t = (string ref * div * div list * div)

let raise_color_samples_exception () =
  let message = "Ot_color_picker.generate_color_samples, \
                 the argument have to be greater than 1" in
  invalid_arg message

let generate_color_samples precision =
  let color_list =
    if precision <= 1 || precision > 256 then raise_color_samples_exception ()
    else
      let step = 255 / (precision - 1) in
      let rec aux_build nl v =
        if (v > 255)
        then nl
        else aux_build ((Printf.sprintf "%02X" v)::nl) (v + step)
      in aux_build [] 0
  in List.map (fun red ->
    List.map (fun green ->
      List.map (fun blue ->
        String.concat "" ["#"; red; green; blue]
      ) color_list ) color_list ) color_list

(* Some pre-generated color samples in several precisions. *)
let color_samples_p2 = lazy (generate_color_samples 2)
let color_samples_p3 = lazy (generate_color_samples 3)
let color_samples_p4 = lazy (generate_color_samples 4)
let color_samples_p5 = lazy (generate_color_samples 5)
let color_samples_p6 = lazy (generate_color_samples 6)

(* Some hand-mained color samples *)
let color_samples_10 = [[["#E03625"; "#FF4B3A"];
                         ["#FF7E02"; "#FFC503"];
                         ["#01CD64"; "#AF58B9"];
                         ["#0198DD"; "#254760"];
                         ["#FFFFFF"; "#000000"]]]

let color_samples_6 = [[["#BEC3C7"; "#7A8E8D"];
                        ["#1C3D50"; "#0280B4"];
                        ["#00A385"; "#A444B2"]]]


(* Take a list of lists of lists of colors (strings) and returns a table list.
   Also returns a div_color_list for launching start script detection. *)
let generate_color_table color_samples =

  let build_color_div color =
    D.div ~a:[a_class["ot-color-picker-square"];
            a_title color;
            a_style ("background-color: " ^ color ^ ";")]
      []
  in
  let build_td_color color_div =
    td ~a:[a_class["ot-color-picker-table-td"]] [color_div]
  in
  let build_tr_color tds =
    tr ~a:[a_class["ot-color-picker-table-tr"]] tds
  in

  let rec build_table div_color_list tables = function
    | []                -> div_color_list, tables
    | head::tail        ->

      let rec build_column div_color_list trs = function
        | []            -> div_color_list, trs
        | head::tail    ->

          let rec build_line div_color_list tds = function
            | []                -> div_color_list, tds
            | color::tail       ->

              let color_div = build_color_div color in
              build_line
                (color_div::div_color_list)
                ((build_td_color color_div)::tds)
                tail
          in

          let div_color_list', tds = build_line div_color_list [] head in
          build_column
            div_color_list'
            ((build_tr_color tds)::trs)
            tail
      in

      let div_color_list', trs = build_column div_color_list [] head in
      let tbl = table ~a:[a_class["ot-color-picker-table"]] trs in
      build_table
        div_color_list'
        (tbl::tables)
        tail
  in

  let div_color_list, tables = build_table [] [] color_samples in
  div_color_list, tables

let make ?(initial_color = 0, 0, 0) ?(color_samples = Lazy.force color_samples_p5) () =
  let tbl, trl, tdl = initial_color in
  let color_ref = ref (List.nth (List.nth (List.nth color_samples tbl) trl) tdl) in
  let div_color_list, tables = generate_color_table color_samples in
  let color_div = D.div ~a:[a_class["ot-color-picker-current-color"];
                          a_title !color_ref;
                          a_style ("background-color: " ^ !color_ref ^ ";")] []
  in
  let block = D.div ~a:[a_class["ot-color-picker-block"]] tables in
  let type_t = (color_ref, color_div, div_color_list, block) in
  type_t, color_div, block

]

[%%client

open Lwt

let fusion (color_ref, color_div, fst_list, block) (_, _, snd_list, _) =
  (color_ref, color_div, fst_list@snd_list, block)

let start (color_ref, color_div, color_list, _) =
  let dom_color_div = Eliom_content.Html.To_dom.of_element color_div in
  let rec aux = function
    | []                -> ()
    | div_elt::tail     ->
      let dom_div = Eliom_content.Html.To_dom.of_element div_elt in
      Lwt.async (fun () ->
        Lwt_js_events.clicks dom_div (fun _ _ ->
          Lwt.return
            (let color = dom_div##.title in
             dom_color_div##.style##.backgroundColor := color;
             dom_color_div##.title := color;
             color_ref := (Js.to_string color))));
      aux tail
  in aux color_list

let generate_and_append (color_ref, color_div, fst_list, block) new_list =
  let div_color_list, tables = generate_color_table new_list in
  let aux = function
    | tbl::t    -> Eliom_content.Html.Manip.appendChild block tbl
    | []        -> ()
  in aux tables;
  div_color_list

let add_square_color color_picker new_list =
  let color_ref, color_div, fst_list, block = color_picker in
  color_ref, color_div,
  fst_list@(generate_and_append color_picker new_list), block

let add_square_color_and_start color_picker new_list =
  let color_ref, color_div, fst_list, block = color_picker in
  ignore (start (color_ref, color_div,
                 generate_and_append color_picker new_list, block))

let get_color (color_ref, _ , _, _) = !color_ref

let get_square_color_div_list (_, _, color_list, _) = color_list

]
