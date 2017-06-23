[%%shared.start]

val make :
  ?dragThreshold:float ->
  ?moveCount:int ->
  ?successClass:string list ->
  ?failureClass:string list ->
  ?pullIconClass:string list ->
  ?loadingIconClass:string list ->
  ?headContainerClass:string list ->
  ?successText:string ->
  ?failureText:string ->
  ?pullDownText:string ->
  ?releaseText:string ->
  ?loadingText:string ->
  ?rotateGradually:bool->
  ?blockPullIcon:bool->
  content: [< Html_types.div_content_fun > `Div ] Eliom_content.Html.elt ->
  (unit-> bool Lwt.t) Eliom_client_value.t ->
  [> Html_types.div ] Eliom_content.Html.D.elt