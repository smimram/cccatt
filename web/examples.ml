let tests = [%blob "../tests/test.cccatt"]
let implicit = [%blob "../tests/implicit.cccatt"]
let curry = [%blob "../examples/curry.cccatt"]
let curien = [%blob "../examples/curien.cccatt"]
let birds = [%blob "../examples/birds.cccatt"]
let linear = [%blob "../examples/linear.cccatt"]
let category = [%blob "../examples/category.cccatt"]

let get = function
  | "tests" -> tests
  | "implicit" -> implicit
  | "curry" -> curry
  | "curien" -> curien
  | "birds" -> birds
  | "linear" -> linear
  | "category" -> category
  | _ -> ""
