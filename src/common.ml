(** Commonly used functions. *)

open Extlib

let print_fun = ref print_string

(** Print a message. *)
let printf fmt = Printf.ksprintf (fun s -> !print_fun s) fmt

(** Return an error message. *)
let error ?pos e =
  let pos =
    match pos with
    | None -> ""
    | Some pos when pos = Pos.dummy -> ""
    | Some pos -> Pos.to_string pos ^ ": "
  in
  Printf.ksprintf (fun s -> printf "[EE]: %s%s.\n%!" pos s; exit 1) e
