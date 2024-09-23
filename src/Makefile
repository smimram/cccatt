all:
	@dune build

test:
	@dune exec ./cccatt.exe test.cccatt

ci:
	git ci . -m "cccatt program."
	git push

explain:
	ocamlc -c extlib.ml value.ml lang.ml && menhir --infer --explain parser.mly && rm *.cmi *.cmo parser.ml parser.mli && less parser.conflicts && rm parser.conflicts
