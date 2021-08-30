let main source_file =
  let error = ref false in
  Py.initialize ();
  let ast = Pyast.python_parse_file source_file in
  let (module Current) = Pyast.get_current_version () in
  let module Parser = Current.Parse (Current) in
  let mod_ = Parser.mod_ ast in
  let parse mod_ (module Target : Pyast.S) =
    let module Convert = Current.To (Target) in
    try
      ignore (Convert.mod_ mod_)
    with e ->
      Printf.eprintf "Error while converting %s to %s: %s\n%s\n"
         (Pyast.Version.to_string Current.version)
         (Pyast.Version.to_string Target.version)
         (Printexc.to_string e)
         (Printexc.get_backtrace ());
      error := true in
  List.iter (parse mod_) Pyast.versions;
  if !error then
    exit 1

let source_file =
  let doc = "Source file" in
  Cmdliner.Arg.(
    required & pos 0 (some non_dir_file) None & info [] ~docv:"FILE" ~doc)

let options = Cmdliner.Term.(const main $ source_file)

let info =
  let doc = "Try to parse a file with all Python versions" in
  let man = [
      `S Cmdliner.Manpage.s_bugs;
      `P "Email bug reports to <thierry.martinez@inria.fr>.";
    ] in
  Cmdliner.Term.info "parse_with_all_versions" ~doc
    ~exits:Cmdliner.Term.default_exits ~man

let () =
    Cmdliner.Term.exit (Cmdliner.Term.eval (options, info))
