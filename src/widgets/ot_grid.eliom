let%shared container ?(attr = []) children =
  Eliom_content.Html.D.(div
    ~a:((a_class ["ot-grid-container"]) :: attr)
    children
  )

let%shared row ?(attr = []) children =
  Eliom_content.Html.D.(div
    ~a:((a_class ["ot-grid-row"]) :: attr)
    children
  )

(* ---------------------------- *)
(* ---------- Column ---------- *)

let%shared string_of_size_list l =
  List.map
  (fun (value, css_property) ->
    if value <> 0 then css_property ^ (string_of_int value) else ""
  )
  l

let%shared col
    ?(xl = 0)
    ?(lg = 0)
    ?(md = 0)
    ?(sm = 0)
    ?(xs = 0)
    ?(attr = [])
    children
  =
  Eliom_content.Html.D.(div
    ~a:((a_class
        (string_of_size_list
          [(xl, "ot-grid-col-xl-") ;
           (lg, "ot-grid-col-lg-") ;
           (md, "ot-grid-col-md-") ;
           (sm, "ot-grid-col-sm-") ;
           (xs, "ot-grid-col-xs-")
          ])
        ) :: attr
       )
    children
  )

(* ---------- Column ---------- *)
(* ---------------------------- *)
