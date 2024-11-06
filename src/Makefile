all:
	@dune build

test:
	@dune exec ./cccatt.exe ../test/test.cccatt

curry:
	@dune exec ./cccatt.exe ../test/curry.cccatt

implicit:
	@dune exec ./cccatt.exe ../test/implicit.cccatt

debug:
	@dune exec ./cccatt.exe debug.cccatt

explain:
	ocamlc -c extlib.ml value.ml lang.ml && menhir --infer --explain parser.mly && rm *.cmi *.cmo parser.ml parser.mli && less parser.conflicts && rm parser.conflicts
