[%%shared.start]

(** an alias for the operator [::]. [@:] has as opposed to [::] the same
    operator precedence as [@] which means that these two can be mixed as for
    instance in the expression [[0] @ 1 @: [2] @ 3 @: [4]] which is also a bit
    more efficient than [[0] @ [1] @ [2] @ [3] @ [4]]. This operator is supposed
    to be used as much as possible when creating lists of HTML elements. *)
val (@:) : 'a -> 'a list -> 'a list

val (@?) : 'a option -> 'a list -> 'a list
