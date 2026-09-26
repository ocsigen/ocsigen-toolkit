# Module `Ot.Icons`

```ocaml
module type S = sig ... end
```
Output of [`Make`](./Ot-Icons-Make.md): a primitive `icon` builder plus a handful of predefined icons for common UI actions.

```ocaml
module Make
  (A : Eliom.Content.Html.T) : 
  S with type 'a elt = 'a A.elt and type 'a attrib = 'a A.attrib
```
Build an icon module on top of an Eliom HTML implementation ([`Eliom.Content.Html.F`](./../../eliom/eliom.client/Eliom-Content-Html-F.md) or [`Eliom.Content.Html.D`](./../../eliom/eliom.client/Eliom-Content-Html-D.md)).

```ocaml
module F : 
  S
    with type 'a elt = 'a Eliom.Content.Html.F.elt
     and type 'a attrib = 'a Eliom.Content.Html.F.attrib
```
Icons built with [`Eliom.Content.Html.F`](./../../eliom/eliom.client/Eliom-Content-Html-F.md) (static HTML).

```ocaml
module D : 
  S
    with type 'a elt = 'a Eliom.Content.Html.D.elt
     and type 'a attrib = 'a Eliom.Content.Html.D.attrib
```
Icons built with [`Eliom.Content.Html.D`](./../../eliom/eliom.client/Eliom-Content-Html-D.md) (HTML with DOM identity, suitable for being referenced from client-side code).
