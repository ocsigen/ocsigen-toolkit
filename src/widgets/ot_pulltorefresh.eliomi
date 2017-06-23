[%%shared.start]

val make :
  ?dragThreshold:float -> 
  (*refresh the page when drag distance/screenHeight > dragThreshold*)
  ?moveCount:int ->
  (*maximal drag distance*)
  ?successClass:string list ->
  (*the class of icon if the function afterPull succeeds*)
  ?failureClass:string list ->
  (*the class of icon if the function afterPull fails*)
  ?pullIconClass:string list ->
  (*the class of icon during dragging*)
  ?loadingIconClass:string list ->
  (*the class of icon during loading*)
  ?headContainerClass:string list ->
  (*the class of head container*)
  ?successText:string ->
  ?failureText:string ->
  ?pullDownText:string ->
  ?releaseText:string ->
  ?loadingText:string ->
  ?rotateGradually:bool->
  (*if the icon rotates gradually during dragging*)
  ?blockPullIcon:bool->
  (*if blockPullIcon=true , the icon can rotate at most 180 deg. Otherwise, it can rotate at most 360deg*)
  content: [< Html_types.div_content_fun > `Div ] Eliom_content.Html.elt ->
  (unit-> bool Lwt.t) Eliom_client_value.t ->
  (*refresh function*)
  [> Html_types.div ] Eliom_content.Html.D.elt

