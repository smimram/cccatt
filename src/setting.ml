(** Global settings. *)

open Extlib
open Common

(** Mode for checking pasting schemes. *)
type mode = [ `Category | `Monoidal | `Symmetric_monoidal | `Symmetric_monoidal_closed | `Cartesian_closed ]

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
          message "setting mode to %s" v;
          match v with
          | "category" -> `Category
          | "cartesian closed" | "ccc" -> `Cartesian_closed
          | "symmetric monoidal" | "smc" -> `Symmetric_monoidal
          | "symmetric monoidal closed" | "smcc" -> `Symmetric_monoidal_closed
          | "monoidal" -> `Monoidal
          | m -> warning "Unknown mode: %s" m; raise Exit
        )
      | k -> warning "Unknown setting: %s" k; raise Exit
  with
  | Exit -> ()
