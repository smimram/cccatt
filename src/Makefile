all:
	@dune build

test:
	$(MAKE) -C .. $@

curry:
	@dune exec ./cccatt.exe ../test/curry.cccatt

implicit:
	@dune exec ./cccatt.exe ../test/implicit.cccatt

debug:
	@dune exec ./cccatt.exe debug.cccatt

explain:
	ocamlc -c extlib.ml common.ml setting.ml term.ml pasting.ml && menhir --infer --explain parser.mly && rm *.cmi *.cmo parser.ml parser.mli && less parser.conflicts && rm parser.conflicts
