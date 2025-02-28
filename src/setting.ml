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
  | `Cartesian
  | `Cartesian_closed
]

let mode = ref (`Cartesian_closed : mode)

(** Whether types have elements. *)
let has_elements () = List.mem !mode [`Cartesian_closed]

let has_hom () = List.mem !mode [`Symmetric_monoidal_closed; `Cartesian_closed]

let has_prod () = List.mem !mode [`Monoidal; `Symmetric_monoidal; `Symmetric_monoidal_closed; `Cartesian; `Cartesian_closed]

let has_one () = has_prod ()

(** Callback when the mode is changed. *)
let mode_callback = ref (fun _ -> ())

let on_mode f =
  let g = !mode_callback in
  mode_callback := fun s -> g s; f s

(** Callback when the dimension is changed. *)
let dim_callback = ref (fun _ -> ())

(** Maximal depth for pasting schemes. *)
let dimension = ref max_int

let on_dim f =
  let g = !dim_callback in
  dim_callback := fun n -> g n; f n

let set_dim n =
  message "setting dimension to %s" (if n = max_int then "∞" else string_of_int n);
  dimension := n;
  !dim_callback n

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
        | "cartesian category" -> `Cartesian
        | "cartesian closed category" | "ccc" -> `Cartesian_closed
        | "symmetric monoidal category" | "smc" -> `Symmetric_monoidal
        | "symmetric monoidal closed category" | "smcc" -> `Symmetric_monoidal_closed
        | "monoidal category" -> `Monoidal
        | m -> error "Unknown mode: %s" m
      );
    !mode_callback !mode
  | "dim" | "dimension" ->
    let n = if v = "oo" || v = "∞" then max_int else int_of_string v in
    set_dim n
  | k -> error "Unknown setting: %s" k

let save, restore =
  let l = ref [] in
  (fun () -> l := (!mode,!dimension) :: !l),
  (fun () -> let m,d = List.hd !l in l := List.tl !l; mode := m; set_dim d)
