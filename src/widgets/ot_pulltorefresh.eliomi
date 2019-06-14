[%%shared.start]

(** Pull to refresh
    This widget can handle any element that needs to be refreshed
    after being pulled down.
*)

(** Represents the state of the gesture behavior.
 `Pulling` happens while the motion hasn't reached the threshold.
 `Ready` happens while releasing the fingers will trigger the event.
 `Succeeded` happens for a short time after a sucessfull event.
 `Failed` happens for a short time after a failed event.
 *)
type state = Pulling | Ready | Loading | Succeeded | Failed

val make
  :  ?a:[< Html_types.div_attrib > `Class] Eliom_content.Html.attrib list
  -> ?dragThreshold:float
  -> ?moveCount:int
  -> ?refresh_timeout:float
  -> ?header:(state option
              -> ([< Html_types.div_content_fun > `Div] as 'a)
                 Eliom_content.Html.elt
                 list)
  -> content:'a Eliom_content.Html.elt
  -> (unit -> bool Lwt.t) Eliom_client_value.t
  -> [> Html_types.div] Eliom_content.Html.elt
(**
   Creates a pull-to-refresh container from an html element.
   [?a] is the attribute array of the returned element
   [?dragThreshold] is a threshold. The container will be refreshed if the
   ratio of the drag distance to the screen height is greater than this
   threshold (default 0.3).
   [?moveCount] is the maximum vertical translation of the container
   (default 200).
   [?refresh_timeout] is the maximum amount of seconds
   to wait for the reload to happen.
   If there is a connection error or some other error,
   (default 20s)
   this duration is how long to wait for a response.
   [?header] is a function defining what to display in
   the space revealed when pulling the page down, depending on the state of
   the gesture.
   IMPORTANT NOTE: Because of the way this module is implemented,
   that space needs a fixed height.
   If you want to give a custom display function, you also need to
   re-style the class `ot-pull-refresh-head-container`
   to override its height and top-margin.
   (default displays a spinner until success or failure)
   [content] is the html element from which the container is created.
   Finally, user needs to provide an [afterPull] function to refresh the
   container.
*)
