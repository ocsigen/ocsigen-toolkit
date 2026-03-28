# Ocsigen Toolkit [![Build](https://github.com/ocsigen/ocsigen-toolkit/actions/workflows/build.yml/badge.svg)](https://github.com/ocsigen/ocsigen-toolkit/actions/workflows/build.yml)

## Introduction

The Ocsigen Toolkit is a set of user interface widgets that facilitate
the development of Eliom applications. The toolkit is in beta state.

## Installation

Use `opam` to install:

```
opam install ocsigen-toolkit
```

**NB:** you may want to include the provided CSS in you own project.
Take a look at the `css` directory for the style files that correspond
to the modules you use.

## Generating API documentation

The API documentation is generated using `eliomdoc` (installed with Eliom)
and the `wikidoc` ocamldoc plugin. Generated wiki files are stored in the
`wikidoc` branch under `doc/dev/api/`.

```bash
# Generate server documentation
eliomdoc -server -ppx -colorize-code -stars -sort \
  -package eliom.server,calendar,js_of_ocaml \
  -i $(ocamlfind query wikidoc) -g odoc_wiki.cma \
  -d _build/doc/server -subproject server \
  src/widgets/*.eliomi

# Generate client documentation
eliomdoc -client -ppx -colorize-code -stars -sort \
  -package eliom.client,calendar,js_of_ocaml,js_of_ocaml-lwt \
  -i $(ocamlfind query wikidoc) -g odoc_wiki.cma \
  -d _build/doc/client -subproject client \
  src/widgets/*.eliomi

# Then copy to the wikidoc branch:
git checkout wikidoc
cp _build/doc/server/*.wiki doc/dev/api/server/
cp _build/doc/client/*.wiki doc/dev/api/client/
```
