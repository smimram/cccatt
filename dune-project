(lang dune 3.13)
(name cccatt)
(version 0.2.0)
(generate_opam_files true)
(source (github smimram/cccatt))
(authors "Samuel Mimram <samuel.mimram@polytechnique.edu>")
(maintainers "Samuel Mimram <samuel.mimram@polytechnique.edu>")
(license GPL-3.0-or-later)
(homepage https://smimram.github.io/cccatt/)
(bug_reports https://github.com/smimram/cccatt/issues)
(documentation https://smimram.github.io/cccatt/)
(using menhir 3.0)

(package
 (name cccatt)
 (synopsis "A typechecker for unbiased cartesian closed categories")
 (description "A typechecker for unbiased cartesian closed categories, based on the CCCaTT type theory. Variants are also supported: categories, cartesian categories, monoidal categories, symmetric monoidal categories, etc.")
 (depends
  ocaml
  dune
  (menhir :build)
  (js_of_ocaml :with-dev-setup)
  (js_of_ocaml-ppx :with-dev-setup)
  (ppx_blob :with-dev-setup)
  (ppx_optcomp :with-dev-setup))
)
