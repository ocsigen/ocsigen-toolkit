let pf = Printf.printf
let spf = Printf.sprintf

let extra_ppx_args = ref []
let subdir_name = ref ""
let server_objs_dir = ref ""

let gen_eliom_ppx_rule ~target ~input ~args =
  let all_args = !extra_ppx_args @ args in
  let target, input_dep =
    if !subdir_name = "" then target, input
    else spf "%s/%s" !subdir_name target, input
  in
  if !subdir_name <> "" then
    pf
      {|(subdir %s
 (rule
  (with-stdout-to %s
   (chdir %%{workspace_root}
    (run ocsigen-ppx-client -as-pp -loc-filename %%{dep:%s} %s %%{dep:%s})))))
|}
      !subdir_name (Filename.basename target) input_dep (String.concat " " all_args) input_dep
  else
    pf
      {|(rule
 (with-stdout-to %s
  (chdir %%{workspace_root}
   (run ocsigen-ppx-client -as-pp -loc-filename %%{dep:%s} %s %%{dep:%s}))))
|}
      target input_dep (String.concat " " all_args) input_dep

let gen_rule_for_module ~server_rel_prefix ~impl fname =
  let target = Filename.basename fname in
  let fname_no_ext = Filename.remove_extension fname in
  let input = Filename.concat server_rel_prefix fname in
  if Filename.extension fname_no_ext = ".pp" then ()
  else
    let args =
      if impl then
        let server_cmo =
          if !server_objs_dir <> "" then
            let module_base = Filename.basename fname_no_ext in
            let cap_name = String.capitalize_ascii module_base in
            let prefix =
              if !subdir_name <> "" then String.lowercase_ascii !subdir_name ^ "__"
              else ""
            in
            let cmo_from_dune_dir =
              spf "%s/%s%s.cmo" !server_objs_dir prefix cap_name
            in
            let cmo_path =
              if !subdir_name <> "" then spf "../%s" cmo_from_dune_dir
              else cmo_from_dune_dir
            in
            spf "%%{dep:%s}" cmo_path
          else
            let server_cmo = Filename.concat server_rel_prefix fname_no_ext in
            spf "%%{cmo:%s}" server_cmo
        in
        [ "--impl"; "-server-cmo"; server_cmo ]
      else [ "--intf" ]
    in
    gen_eliom_ppx_rule ~target ~input ~args

(** Relative path to server modules from the generated client modules. *)
let server_rel_prefix () = ".."

let gen_rule_for_file fname =
  let server_rel_prefix = server_rel_prefix () in
  match Filename.extension fname with
  | ".eliom" -> gen_rule_for_module ~server_rel_prefix ~impl:true fname
  | ".eliomi" -> gen_rule_for_module ~server_rel_prefix ~impl:false fname
  | _ -> ()

let run files = List.iter gen_rule_for_file files
