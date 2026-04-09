let run internal_prefix subdir server_objs_dir dir =
  (match internal_prefix with
   | Some p -> Gen_rules.extra_ppx_args := ["-internal-prefix"; p]
   | None -> ());
  (match subdir with
   | Some s -> Gen_rules.subdir_name := s
   | None -> ());
  (match server_objs_dir with
   | Some d -> Gen_rules.server_objs_dir := d
   | None -> ());
  let files = Utils.list_dir dir in
  let files = List.filter (Fun.negate Utils.is_dir) files in
  Gen_rules.run files

open Cmdliner

let arg_dir =
  let doc = "Directory containing the Eliom modules." in
  Arg.(required & pos 0 (some dir) None & info ~doc ~docv:"DIR" [])

let arg_internal_prefix =
  let doc = "Strip $(docv). wrapper prefix from .cmo type paths (for compiling wrapped libraries)." in
  Arg.(value & opt (some string) None & info ~doc ~docv:"PREFIX" ["internal-prefix"])

let arg_subdir =
  let doc = "Put generated files in $(docv)/ subdirectory (for include_subdirs qualified)." in
  Arg.(value & opt (some string) None & info ~doc ~docv:"DIR" ["subdir"])

let arg_server_objs_dir =
  let doc =
    "Use direct paths to server .cmo files in $(docv) instead of %%{cmo:...}. \
     $(docv) is the path to the server library's .objs/byte/ directory, \
     relative to the dune file. The module prefix is derived from --subdir."
  in
  Arg.(value & opt (some string) None & info ~doc ~docv:"DIR" ["server-objs-dir"])

let cmd =
  let term = Term.(const run $ arg_internal_prefix $ arg_subdir $ arg_server_objs_dir $ arg_dir) in
  let doc =
    "Generate dune rules for building an ocsigen application or library."
  in
  let info = Cmd.info "ocsigen-dune-rules" ~version:"%%VERSION%%" ~doc in
  Cmd.v info term

let () = exit (Cmd.eval cmd)
