open Extlib
open Common

type mode = [ `Cartesian_closed | `Symmetric_monoidal | `Monoidal ]

let mode = ref (`Cartesian_closed : mode)

let parse s =
  let k, v = String.split_on_first_char ':' s in
  let k = String.trim k in
  let v = String.trim v in
  try
    mode :=
      match k with
      | "mode" ->
        (
          match v with
          | "cartesian closed" -> `Cartesian_closed
          | "symmetric monoidal" -> `Symmetric_monoidal
          | "monoidal" -> `Monoidal
          | m -> warning "Unknown mode: %s" m; raise Exit
        )
      | k -> warning "Unknown setting: %s" k; raise Exit
  with
  | Exit -> ()
