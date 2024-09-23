open Extlib

let error ?pos e =
  let pos =
    match pos with
    | None -> ""
    | Some pos when pos = Pos.dummy -> ""
    | Some pos -> Pos.to_string pos ^ ": "
  in
  Printf.ksprintf (fun s -> Printf.printf "[EE]: %s%s.\n%!" pos s; exit 1) e
