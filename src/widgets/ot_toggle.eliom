{shared{
module Html5 = Eliom_content.Html5
open Html5.F
}}

{shared{

type t = T_Up | T_Down

}}

{shared{

    let display_toggle
        ?up_txt:(up_txt = "up")
        ?down_txt:(down_txt = "down") f =
      function
      | T_Up ->
        div ~a:[a_class ["ot-toggle"]]
          [div ~a:[a_class ["ot-active"; "ot-up"]]
             [pcdata up_txt];
           div ~a:[a_class ["ot-inactive"; "ot-down"];
                   a_onclick {{ fun _ -> %f T_Down }}]
             [pcdata down_txt]]
      | T_Down ->
        div ~a:[a_class ["ot-toggle"]]
          [div ~a:[a_class ["ot-inactive"; "ot-up"];
                   a_onclick {{ fun _ -> %f T_Up }}]
             [pcdata up_txt];
           div ~a:[a_class ["ot-active"; "ot-down"]]
             [pcdata down_txt]]

let is_up = function
  | T_Up ->
    true
  | T_Down ->
    false

}}

{server{

    let display ?init_up:(init_up = false) ?up_txt ?down_txt () =
      let e, f =
        (if init_up then T_Up else T_Down) |>
        Eliom_csreact.SharedReact.S.create
      in
      let d =
        Eliom_csreact.SharedReact.S.map
          (Eliom_lib.create_shared_value
             (display_toggle f)
             {{display_toggle %f}})
          e
      and is_up =
        Eliom_csreact.SharedReact.S.map
          (Eliom_lib.create_shared_value
             is_up
             {{is_up}})
          e
      in
      d, is_up

}}
