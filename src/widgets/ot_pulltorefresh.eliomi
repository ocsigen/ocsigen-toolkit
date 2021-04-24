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
  -> ?app_only:bool
  -> ?scale:float
  -> ?dragThreshold:float
  -> ?refresh_timeout:float
  -> ?header:
       (state option
        -> ([< Html_types.div_content_fun > `Div] as 'a) Eliom_content.Html.elt
           list)
       Eliom_shared.Value.t
  -> content:'a Eliom_content.Html.elt
  -> (unit -> bool Lwt.t) Eliom_client_value.t
  -> 'a Eliom_content.Html.elt
(**
   Creates a pull-to-refresh container from an html element.
   [?a] is the attribute array of the returned element
   [?app_only] specifies whether to activate the behavior only in the mobile app
   or also do it in a browser. Useful if you want refreshable contents
   in your page that should also work inside a mobile browser.
   (default true)
   [?scale] is the scaling factor of the drag motion.
   Higher values means the page will follow the motion of the finger
   more closely.
   (default 5)
   [?dragThreshold] is a threshold. The container will be refreshed if the
   motion distance goes above the specified threshold
   (default 80px).
   [?refresh_timeout] is the maximum amount of seconds
   to wait for the reload to happen.
   If there is a connection error or some other error,
   this duration is how long to wait for a response.
   (default 20s)
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
