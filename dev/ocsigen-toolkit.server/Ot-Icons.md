# Module `Ot.Icons`

Helpers to build `<i>` icon elements styled by the toolkit's CSS.

Each icon is rendered as an empty `<i class="ot-icon ...">` element whose CSS classes select the actual glyph. The toolkit ships an `ot_icons.css` stylesheet that defines all the `ot-icon-*` and `ot-*` classes used by the predefined icons below. Include that file (or your own equivalent) in your application for the icons to actually render.

Use [`Ot.Icons.F`](./Ot-Icons-F.md) for static HTML and [`Ot.Icons.D`](./Ot-Icons-D.md) for HTML with DOM identity (typically when you need to refer to the produced element from client code), the same way Eliom exposes [`Eliom.Content.Html.F`](./../../eliom/eliom.server/Eliom-Content-Html-F.md) and [`Eliom.Content.Html.D`](./../../eliom/eliom.server/Eliom-Content-Html-D.md).

```ocaml
module type S = sig ... end
```
Output of [`Make`](./Ot-Icons-Make.md): a primitive `icon` builder plus a handful of predefined icons for common UI actions.

```ocaml
module Make
  (A : Eliom.Content.Html.T) : 
  S with type 'a elt = 'a A.elt and type 'a attrib = 'a A.attrib
```
Build an icon module on top of an Eliom HTML implementation ([`Eliom.Content.Html.F`](./../../eliom/eliom.server/Eliom-Content-Html-F.md) or [`Eliom.Content.Html.D`](./../../eliom/eliom.server/Eliom-Content-Html-D.md)).

```ocaml
module F : 
  S
    with type 'a elt = 'a Eliom.Content.Html.F.elt
     and type 'a attrib = 'a Eliom.Content.Html.F.attrib
```
Icons built with [`Eliom.Content.Html.F`](./../../eliom/eliom.server/Eliom-Content-Html-F.md) (static HTML).

```ocaml
module D : 
  S
    with type 'a elt = 'a Eliom.Content.Html.D.elt
     and type 'a attrib = 'a Eliom.Content.Html.D.attrib
```
Icons built with [`Eliom.Content.Html.D`](./../../eliom/eliom.server/Eliom-Content-Html-D.md) (HTML with DOM identity, suitable for being referenced from client-side code).
