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

(** Callback when the mode is changed. *)
let mode_callback = ref (fun _ -> ())

let on_mode f =
  let g = !mode_callback in
  mode_callback := fun s -> g s; f s

(** Maximal depth for pasting schemes. *)
let depth = ref None

let parse s =
  let k, v = String.split_on_first_char ':' s in
  let k = String.trim k in
  let v = String.trim v in
  match k with
  | "mode" ->
    message "setting mode to %s" v;
    mode :=
      (
        match v with
        | "plain" -> `Plain
        | "monoid" -> `Monoid
        | "category" -> `Category
        | "cartesian closed category" | "ccc" -> `Cartesian_closed
        | "symmetric monoidal category" | "smc" -> `Symmetric_monoidal
        | "symmetric monoidal closed category" | "smcc" -> `Symmetric_monoidal_closed
        | "monoidal category" -> `Monoidal
        | m -> error "Unknown mode: %s" m
      );
    !mode_callback !mode
  | "depth" ->
    let n = int_of_string v in
    message "setting depth to %d" n;
    depth := Some n
  | k -> error "Unknown setting: %s" k
