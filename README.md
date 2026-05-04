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

Wiki API pages are generated from the `.eliomi` files using `eliomdoc`
and the [wikidoc](https://github.com/ocsigen/wikidoc) plugin:

```bash
bash build/gen_wikidoc.sh        # all (server + client)
bash build/gen_wikidoc.sh server # server side only
bash build/gen_wikidoc.sh client # client side only
```

Output goes to `_build/doc/dev/api/{server,client}/`.

The wiki files are published on the [wikidoc branch] under
`doc/dev/api/{server,client}/`. To update the published documentation,
copy the generated `.wiki` files to a checkout of the `wikidoc` branch
and commit.

[wikidoc branch]: https://github.com/ocsigen/ocsigen-toolkit/tree/wikidoc/doc/dev/api
