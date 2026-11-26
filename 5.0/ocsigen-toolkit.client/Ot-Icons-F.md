# Module `Icons.F`

Icons built with [`Eliom.Content.Html.F`](./../../eliom/eliom.server/Eliom-Content-Html-F.md) (static HTML).

```ocaml
type 'a elt = 'a Eliom.Content.Html.F.elt
```
```ocaml
type 'a attrib = 'a Eliom.Content.Html.F.attrib
```
```ocaml
val icon : 
  string list ->
  ?a:Html_types.i_attrib attrib list ->
  unit ->
  [> Html_types.i ] elt
```
`icon classes ()` is an empty `<i>` element whose CSS classes are `"ot-icon"` followed by `classes`. The optional `?a` argument lets the caller add extra attributes (typically more classes). It comes last so that the predefined icons below can be partially applied and still expose `?a`.

```ocaml
val user : ?a:Html_types.i_attrib attrib list -> unit -> [> Html_types.i ] elt
```
Predefined icon for a user / profile glyph (CSS class `ot-icon-user`).

```ocaml
val plus : ?a:Html_types.i_attrib attrib list -> unit -> [> Html_types.i ] elt
```
Predefined "plus" / add icon (CSS class `ot-plus`).

```ocaml
val spinner : 
  ?a:Html_types.i_attrib attrib list ->
  unit ->
  [> Html_types.i ] elt
```
Animated spinner icon (CSS classes `ot-icon-spinner ot-icon-animation-spinning`).

```ocaml
val shutdown : 
  ?a:Html_types.i_attrib attrib list ->
  unit ->
  [> Html_types.i ] elt
```
Shutdown / power icon (CSS class `ot-icon-power`).

```ocaml
val config : 
  ?a:Html_types.i_attrib attrib list ->
  unit ->
  [> Html_types.i ] elt
```
Configuration / gear icon (CSS class `ot-icon-gear`).

```ocaml
val signout : 
  ?a:Html_types.i_attrib attrib list ->
  unit ->
  [> Html_types.i ] elt
```
Sign out / logout icon (CSS class `ot-icon-sign-out`).

```ocaml
val close : ?a:Html_types.i_attrib attrib list -> unit -> [> Html_types.i ] elt
```
Close icon (CSS class `ot-icon-close`).

```ocaml
val question : 
  ?a:Html_types.i_attrib attrib list ->
  unit ->
  [> Html_types.i ] elt
```
Question mark / help icon (CSS class `ot-icon-question`).
