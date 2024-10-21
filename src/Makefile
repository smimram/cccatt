all:
	@dune build

test:
	@dune exec ./cccatt.exe test.cccatt

debug:
	@dune exec ./cccatt.exe debug.cccatt

explain:
	ocamlc -c extlib.ml value.ml lang.ml && menhir --infer --explain parser.mly && rm *.cmi *.cmo parser.ml parser.mli && less parser.conflicts && rm parser.conflicts
