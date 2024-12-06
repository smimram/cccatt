open Extlib
open Common

type mode = [ `Cartesian_closed ]

let mode = ref (`Cartesian_closed : mode)

let parse s =
  let k, v = String.split_on_first_char ':' s in
  let k = String.trim k in
  let v = String.trim v in
  match k with
  | "mode" ->
    (
      match v with
      | "cartesian closed" -> mode := `Cartesian_closed
      | m -> warning "Unknown mode: %s" m
    )
  | k -> warning "Unknown setting: %s" k
