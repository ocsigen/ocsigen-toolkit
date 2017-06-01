[%%shared.start]

(** Ot_color_picker : this module allows to generate, client side, a color
    selector. This selector consists of a color table and an html div that
    displays the current selected color. *)

(** The main type of Ot_color_picker module *)
type t
type div = Html_types.div Eliom_content.Html.D.elt

(** The argument is the divisor of 255. It has to be greater than 1 *)
val generate_color_samples : int -> string list list list

(** Some pre-generated color samples in several precision. Color samples
    are list of lists of lists of colors represented in string of hexadecimal
    values.*)

val color_samples_p2 : string list list list Lazy.t
val color_samples_p3 : string list list list Lazy.t
val color_samples_p4 : string list list list Lazy.t
val color_samples_p5 : string list list list Lazy.t
val color_samples_p6 : string list list list Lazy.t

(** Some hand-mained color samples. *)

val color_samples_6 : string list list list (* 1 table 2 columns 5 lines *)
val color_samples_10 : string list list list (* 1 table 2 columns 3 lines *)

(** Take one list (tables) of lists (columns) of lists (lines) of colors (string)
    and builds the table of colors with it.
    By default this list is initialised with color_samples_p5

    It returns
    - t for future actions,
    - color_div, to display the currently selected color,
        it is not mandatory to include it in the page
    - and the block with all the color squares in the generated table *)
val make :
  ?initial_color: int * int * int ->
  ?color_samples: string list list list ->
  unit ->
  (t * div * div)



[%%client.start]

(** Get two color pickers to fusion in a single one. This new color picker uses
    the color squares of both.
    It uses color_div of the first color picker given in argument. It also keeps
    a reference to the first color picker's block for appending a color in the
    future.
    This action has to be done before using the start function *)
val fusion : t -> t -> t

(** It allows to add square color and to append directly in the block that
    contains the square colors.
    It has to be made before start *)
val add_square_color : t -> string list list list -> t

(** Launch listeners *)
val start : t -> unit

(** It allows to add square color after the start function. It have not to be
    used before start *)
val add_square_color_and_start : t -> string list list list -> unit

(** Get the currently selected color of the selector. The fusion or add_square
    functions have no effects on it. *)
val get_color: t -> string

(** get all square color div element *)
val get_square_color_div_list : t -> div list
