
# Module `Ot_icons.Make`


## Parameters

```ocaml
module A : module type of Eliom_content.Html.F
```

## Signature

```ocaml
val icon : 
  Html_types.nmtoken list ->
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
`icon classes` create an icon HTML attribute with "ot-icon" and `classes` \* as CSS classes. \* The optional parameter is at the end to be able to add other CSS classes \* with predefined icons.

```ocaml
val user : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
```ocaml
val plus : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
```ocaml
val spinner : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
```ocaml
val shutdown : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
```ocaml
val config : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
```ocaml
val signout : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
```ocaml
val close : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```
```ocaml
val question : 
  ?a:Html_types.i_attrib A.attrib list ->
  unit ->
  [> Html_types.i ] A.elt
```