let tests = [%blob "../test/test.cccatt"]
let implicit = [%blob "../test/implicit.cccatt"]
let curry = [%blob "../test/curry.cccatt"]
let curien = [%blob "../test/curien.cccatt"]

let get = function
  | "tests" -> tests
  | "implicit" -> implicit
  | "curry" -> curry
  | "curien" -> curien
  | _ -> ""
