(** Global settings. *)

open Extlib
open Common

(** Mode for checking pasting schemes. *)
type mode = [
  | `Plain
  | `Monoid
  | `Category
  | `Monoidal
  | `Symmetric_monoidal
  | `Symmetric_monoidal_closed
  | `Cartesian_closed
]

let mode = ref (`Cartesian_closed : mode)

(** Maximal depth for pasting schemes. *)
let depth = ref None

let parse s =
  let k, v = String.split_on_first_char ':' s in
  let k = String.trim k in
  let v = String.trim v in
  try
    match k with
    | "mode" ->
      message "setting mode to %s" v;
      mode :=
        (
          match v with
          | "plain" -> `Plain
          | "monoid" -> `Monoid
          | "category" -> `Category
          | "cartesian closed" | "ccc" -> `Cartesian_closed
          | "symmetric monoidal" | "smc" -> `Symmetric_monoidal
          | "symmetric monoidal closed" | "smcc" -> `Symmetric_monoidal_closed
          | "monoidal" -> `Monoidal
          | m -> warning "Unknown mode: %s" m; raise Exit
        )
    | "depth" ->
      let n = int_of_string v in
      message "setting depth to %d" n;
      depth := Some n
    | k -> warning "Unknown setting: %s" k; raise Exit
  with
  | Exit -> ()
