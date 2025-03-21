open Common

let usage = "cccatt [options] [file]"

let () =
  Printexc.record_backtrace true;
  let files = ref [] in
  Arg.parse
    (Arg.align
       [
         "--disable-pasting", Arg.Set Settings.disable_pasting_check, " Disable checking of pasting schemes.";
       ]
    )
    (fun s -> files := s::!files)
    usage;
  if !files = [] then (print_endline usage; exit 0);
  try
    List.fold_left
      (fun envs f ->
         print_newline ();
         message "checking %s" f;
         Lang.exec envs (Lang.parse_file f)
      ) ([], []) !files |> ignore;
    info "used %d metavariables" !Term.meta_counter
  with
  | e ->
    let bt = Printexc.get_raw_backtrace () |> Printexc.raw_backtrace_to_string in
    let e =
      match e with
      | Failure e -> e
      | e -> Printexc.to_string e
    in
    Printf.printf "\nError: %s\n\n%s%!" e bt;
    exit 1
