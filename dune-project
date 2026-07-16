(lang dune 3.13)
(name cccatt)
(version 0.2.0)
(generate_opam_files true)
(license GPL-3.0)
(using menhir 3.0)

(package
 (name cccatt)
 (synopsis "A typechecker for unbiased cartesian closed categories")
 (description "A typechecker for unbiased cartesian closed categories, based on the CCCaTT type theory. Variants are also supported: categories, cartesian categories, monoidal categories, symmetric monoidal categories, etc.")
 (depends ocaml dune)
)
