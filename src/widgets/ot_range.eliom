{shared{
module Html5 = Eliom_content.Html5
open Html5.F
}}

{shared{

type t = int * int * string array

type irs = int React.signal

type irp = irs * (?step:React.step -> int -> unit)

}} ;;

{client{

    let display_aux (_, _, a) v =
      div ~a:[a_class ["ot-r-value"]] [pcdata a.(v)]

let go_up (lb, ub, a) ((r, f) : irp) =
  let v = Eliom_csreact.React.S.value r in
  assert (v <= ub - 1);
  f (if v = ub - 1 then lb else v + 1)

let go_down (lb, ub, a) ((r, f) : irp) =
  let v = Eliom_csreact.React.S.value r in
  assert (v >= lb);
  f (if v = lb then ub - 1 else v - 1)

}} ;;

{shared{

    let display
        ?txt_up:(txt_up = "up")
        ?txt_down:(txt_down = "down")
        (e : t) r =
      div ~a:[a_class ["ot-range"]]
        [div ~a:[a_class ["ot-r-up"];
                 a_onclick {{ fun _ -> go_up %e %r }}]
           [pcdata txt_up];
         Eliom_content.Html5.C.node
           {{ Eliom_content.Html5.R.node
                (Eliom_csreact.React.S.map
                   (display_aux %e)
                   (fst %r)) }};
         div ~a:[a_class ["ot-r-down"];
                 a_onclick {{ fun _ -> go_down %e %r }}]
           [pcdata txt_down]]

let make ?txt_up ?txt_down ?lb:(lb = 0) ~ub f =
  assert (ub > lb);
  let a =
    let f i = f (i + lb) in
    Array.init (ub - lb) f
  and r = {irp{ Eliom_csreact.React.S.create %lb }} in
  display ?txt_up ?txt_down (lb, ub, a) r, {irs{ fst %r }}

}}
