let usage = "cccatt [options] [file]"

let () =
  Printexc.record_backtrace true;
  let files = ref [] in
  Arg.parse
    []
    (fun s -> files := s::!files)
    usage;
  if !files = [] then (print_endline usage; exit 0);
  List.fold_left (fun envs f -> Lang.exec envs (Prover.parse_file f)) ([], []) !files |> ignore
