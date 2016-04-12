[%%client.start]

let rec node_in_document node =
  node == (Dom_html.document :> Dom.node Js.t) ||
  Js.Opt.case (node##.parentNode) (fun () -> false) node_in_document

let watched = ref []

let handler records observer =
  let changes = ref false in
  for i = 0 to records##.length - 1 do
    Js.Optdef.iter (Js.array_get records i)
      (fun r -> if r##.addedNodes##.length > 0 then changes := true)
  done;
  if !changes then begin
    let (ready, not_ready) =
      List.partition (fun (n, _) -> node_in_document n) !watched in
    watched := not_ready;
    if not_ready = [] then observer##disconnect;
    List.iter (fun (_, s) -> Lwt.wakeup s ()) ready
  end

let observer =
  new%js MutationObserver.mutationObserver(Js.wrap_callback handler)

let config =
  let cfg = MutationObserver.empty_mutation_observer_init () in
  cfg##.childList := true;
  cfg##.subtree := true;
  cfg

let nodeready node =
  let node = (node :> Dom.node Js.t) in
  if node_in_document node then Lwt.return () else begin
    let t, s = Lwt.wait () in
    if !watched = [] then observer##observe Dom_html.document config;
    watched := (node, s) :: !watched;
    t
  end
