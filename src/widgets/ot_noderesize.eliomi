
[%%client.start]
type resize_sensor

val attach : #Dom_html.element Js.t -> resize_sensor
val noderesize : resize_sensor -> (unit -> unit) -> unit
val detach : resize_sensor -> unit
