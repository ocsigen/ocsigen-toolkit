(lang dune 3.18)

(name ocsigen-toolkit)

(generate_opam_files true)

(source
 (github ocsigen/ocsigen-toolkit))

(homepage "http://www.ocsigen.org")

(authors "dev@ocsigen.org")

(maintainers "dev@ocsigen.org")

(license "LGPL-2.1-only WITH OCaml-LGPL-linking-exception")

(maintenance_intent "(latest)")

(dialect
 (name "eliom-server")
 (implementation
  (extension "eliom"))
 (interface
  (extension "eliomi")))

(package
 (name ocsigen-toolkit)
 (synopsis
  "Reusable UI components for Eliom applications (client only, or client-server)")
 (description
  "The Ocsigen Toolkit is a set of user interface widgets that facilitate the development of Eliom applications.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (js_of_ocaml
   (>= "6.0.0"))
  (eliom
   (>= "11.0.0"))
  (calendar
   (>= "2.0.0"))
  cmdliner))
