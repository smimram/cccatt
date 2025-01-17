let tests = [%blob "../tests/test.cccatt"]
let implicit = [%blob "../tests/implicit.cccatt"]
let curry = [%blob "../examples/curry.cccatt"]
let curien = [%blob "../examples/curien.cccatt"]
let birds = [%blob "../examples/birds.cccatt"]
let linear = [%blob "../examples/linear.cccatt"]
let category = [%blob "../examples/category.cccatt"]
let monoidal = [%blob "../examples/monoidal.cccatt"]
let symmetric = [%blob "../examples/symmetric.cccatt"]

let get = function
  | "none" -> ""
  | "tests" -> tests
  | "implicit" -> implicit
  | "curry" -> curry
  | "curien" -> curien
  | "birds" -> birds
  | "linear" -> linear
  | "category" -> category
  | "monoidal" -> monoidal
  | "symmetric" -> symmetric
  | _ -> ""
