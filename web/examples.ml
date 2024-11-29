let tests = [%blob "../tests/test.cccatt"]
let implicit = [%blob "../tests/implicit.cccatt"]
let curry = [%blob "../examples/curry.cccatt"]
let curien = [%blob "../examples/curien.cccatt"]

let get = function
  | "tests" -> tests
  | "implicit" -> implicit
  | "curry" -> curry
  | "curien" -> curien
  | _ -> ""
