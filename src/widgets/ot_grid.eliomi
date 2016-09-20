(** This module provides function to create a responsive grid view with
    container, row and column.

    The grid is based on 12 columns and supports multiple screen width:
      xs     : extra small screen -> under 553px.
      small  : small screen       -> between 554px and 767px
      md     : medium screen      -> between 768px and 991px
      lg     : large screen       -> between 992px and 1199px
      xl     : extra large screen -> above 1200px

    It depends on the CSS file ot_grid.css.

    Based on http://www.w3schools.com/css/css_rwd_grid.asp.
 *)

(** [container ~attr children] creates a container (a div with the CSS class
    « ot-container »).

    @param attr additional attributes.
    @param children the children elements.
 *)
val container :
  ?attr:Html_types.div_attrib Eliom_content.Html.attrib list  ->
  [< Html_types.div_content ] Eliom_content.Html.elt list ->
  Html_types.div_content Eliom_content.Html.elt

(** [row ~attr children] creates a row (a div with the CSS class « ot-row »).

    @param attr additional attributes.
    @param children the children elements.
 *)
val row :
  ?attr:Html_types.div_attrib Eliom_content.Html.attrib list ->
  [< Html_types.div_content ] Eliom_content.Html.elt list ->
  Html_types.div_content Eliom_content.Html.elt

(** [col ~xl ~lg ~md ~sm ~xs ~attr children] creates a div element corresponding
    to [xl] (resp. [lg], [md], [sm] and [xs]) columns on extra large screen
    (resp. large screen, medium screen, small screen, extra small screen).

    By default, if the number of columns is not set for a screen size, it takes
    the size of the previous smaller screen. (for example, if [md] is mentionned
    but not [lg], the number of columns for large screen will be [md].
 *)
val col :
  ?xl:int ->
  ?lg:int ->
  ?md:int ->
  ?sm:int ->
  ?xs:int ->
  ?attr:Html_types.div_attrib Eliom_content.Html.attrib list ->
  [< Html_types.div_content ] Eliom_content.Html.elt list ->
  Html_types.div_content Eliom_content.Html.elt
