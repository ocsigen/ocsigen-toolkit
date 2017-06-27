[%%shared.start]

(** Pull to refresh
    This widget can handle any element that needs to be refreshed 
    after being pulled down. 
*)

(**
   Creates a pull-to-refresh container from an html element.
   [?dragThreshold] is a threshold. The container will be refreshed if the 
   ratio of the drag distance to the screen height is greater than this 
   threshold (default 0.3).
   [?moveCount] is the maximum vertical translation of the container 
   (default 200).
   [?pullDownIcon] is the icon shown when the container is pulled down. It 
   is an arrow icon by default. 
   [?loadingIcon] is the icon shown when the container is being refreshed. 
   It is a spinner by default.
   [?successIcon] is the icon shown when the container is successfully
   refreshed. It is a green tick mark by default. 
   [?failureIcon] is the icon shown when the container is not successfully
   refreshed. It is an exclamatory mark by default.
   If you don't want to display certain icons, just pass an empty div as
   parameter.
   [?pullText] contains the text shown during different phases of 
   "pull-to-refresh". It is a span element by default.
   [?headContainer] is the container enveloping the 4 icons and [pullText]
   [?successText] is the information shown when the container is successfully
   refreshed. 
   [?failureText] is the information shown when the container refresh fails.
   [?pullDownText] is the information shown when the container is pulled down.
   [?releaseText] is the information shown when the container is pulled down 
   and the ratio of its translation to the screen height is greater than 
   dragThreshold
   [?loadingText] is the information shown during the loading. 
   Set [?rotateGradually] to true if you want [?pullDownIcon] rotates 
   gradually when the container moves. 
   Set [?blockPullIcon] to false to enable [?pullDownIcon] to rotate up to 
   360 degrees.
   [?alreadyAdded] is [false] by default. If it is true, this means you have
   already added all icons [?pullText] into [?headContainer]
   [content] is the html element from which the container is created. 
   Finally, user needs to provide an [afterPull] function to refresh the 
   container. 
*)
val make :
  ?dragThreshold: float -> 
  ?moveCount: int ->
  ?pullDownIcon: Html_types.div Eliom_content.Html.D.elt ->
  ?loadingIcon: Html_types.div Eliom_content.Html.D.elt ->
  ?successIcon: Html_types.div Eliom_content.Html.D.elt ->
  ?failureIcon: Html_types.div Eliom_content.Html.D.elt ->
  ?pullText: Html_types.span Eliom_content.Html.D.elt ->
  ?headContainer: ([< Html_types.div_content_fun > `Div ] as 'a)
    Eliom_content.Html.D.elt ->
  ?successText: string ->
  ?failureText: string ->
  ?pullDownText: string ->
  ?releaseText: string ->
  ?loadingText: string ->
  ?rotateGradually: bool->
  ?blockPullIcon: bool->
  ?alreadyAdded: bool ->
  content: 'a Eliom_content.Html.elt ->
  (unit-> bool Lwt.t) Eliom_client_value.t ->
  [> Html_types.div ] Eliom_content.Html.D.elt
