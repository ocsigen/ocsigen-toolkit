let pf = Printf.printf
let spf = Printf.sprintf

let gen_eliom_ppx_rule ~target ~input ~impl ~extra_args =
  let impl = if impl then "--impl" else "--intf" in
  (* Not using [--as-pp] because that generates code with this error:
     {v
      File "../ot_buttons.eliom", line 1:    
      Error: Unbound value __eliom__compilation_unit_id__X25vbm
     v} *)
  pf
    {|(rule
 (with-stdout-to %s
  (run ../tools/eliom_ppx_client.exe %s %s %%{dep:%s})))
  |}
    target
    (String.concat " " extra_args)
    impl input

let handle_file_client nm =
  let target_base = Filename.basename nm and input = Filename.concat ".." nm in
  if Filename.check_suffix nm ".pp.eliom"
  then ()
  else if Filename.check_suffix nm ".pp.eliomi"
  then ()
  else if Filename.check_suffix nm ".eliom"
  then
    let target = Filename.chop_extension target_base ^ ".ml" in
    let server_cmo = Filename.chop_extension (Filename.concat ".." nm) in
    gen_eliom_ppx_rule ~target ~input ~impl:true
      ~extra_args:["-server-cmo"; spf "%%{cmo:%s}" server_cmo]
  else if Filename.check_suffix nm ".eliomi"
  then
    let target = Filename.chop_extension target_base ^ ".mli" in
    gen_eliom_ppx_rule ~target ~input ~impl:false ~extra_args:[]

let read_dir d =
  (* Dune doesn't accept paths like [.././module.cmo]. *)
  let concat_dir d = if d = "." then Fun.id else Filename.concat d in
  Sys.readdir d |> Array.map (concat_dir d)

let () =
  let args = List.tl (Array.to_list Sys.argv) in
  let files = Array.concat (List.map read_dir args) in
  Array.sort String.compare files;
  Array.iter handle_file_client files
