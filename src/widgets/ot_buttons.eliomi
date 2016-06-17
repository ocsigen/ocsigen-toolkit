[%%shared.start]

open Html_types
open Eliom_content.Html


val dropdown :
  ?a:div_attrib attrib list ->
  menu:div_content elt list ->
  div_content elt list ->
  div_content elt
