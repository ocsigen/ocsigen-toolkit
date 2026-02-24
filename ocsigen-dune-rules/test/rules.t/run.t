  $ ocsigen-dune-rules .
  (rule
   (with-stdout-to a.ml
    (run ocsigen-ppx-client --impl -server-cmo %{cmo:../a} %{dep:../a.eliom})))
    (rule
   (with-stdout-to b.ml
    (run ocsigen-ppx-client --impl -server-cmo %{cmo:../b} %{dep:../b.eliom})))
    (rule
   (with-stdout-to b.mli
    (run ocsigen-ppx-client --intf %{dep:../b.eliomi})))
    
