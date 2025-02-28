let tests = [%blob "../tests/test.cccatt"]
let implicit = [%blob "../tests/implicit.cccatt"]
let ccc = [%blob "../examples/ccc.cccatt"]
let ccc2 = [%blob "../examples/ccc2.cccatt"]
let curry = [%blob "../examples/curry.cccatt"]
let curien = [%blob "../examples/curien.cccatt"]
let birds = [%blob "../examples/birds.cccatt"]
let linear = [%blob "../examples/linear.cccatt"]
let category = [%blob "../examples/category.cccatt"]
let monoidal = [%blob "../examples/monoidal.cccatt"]
let symmetric = [%blob "../examples/symmetric.cccatt"]
let cartesian = [%blob "../examples/cartesian.cccatt"]
let compact_closed = [%blob "../examples/compact-closed.cccatt"]
let bicategory = [%blob "../examples/bicategory.cccatt"]
let cartesian_bicategory = [%blob "../examples/bicategory-cartesian.cccatt"]

let () =
  Js_of_ocaml.Sys_js.create_file ~name:"monoidal.cccatt" ~content:monoidal;
  Js_of_ocaml.Sys_js.create_file ~name:"bicategory.cccatt" ~content:bicategory

let get = function
  | "none" -> ""
  | "tests" -> tests
  | "implicit" -> implicit
  | "ccc" -> ccc
  | "ccc2" -> ccc2
  | "curry" -> curry
  | "curien" -> curien
  | "birds" -> birds
  | "linear" -> linear
  | "category" -> category
  | "monoidal" -> monoidal
  | "symmetric" -> symmetric
  | "cartesian" -> cartesian
  | "compact-closed" -> compact_closed
  | "bicategory" -> bicategory
  | "cartesian-bicategory" -> cartesian_bicategory
  | _ -> ""
