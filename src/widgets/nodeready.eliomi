(* Wait for a node to be inserted in the DOM.
   Be aware that using it on a node that is never actually added in the DOM
   will make the node and the thread wakener kept in memory.
   Also, note that nodeready is fired only once (except if you add a new
   listener to it).

   How to:
   let _ = nodeready node in Firebug.console##debug node
*)
[%%client.start]

val nodeready : #Dom.node Js.t -> unit Lwt.t
