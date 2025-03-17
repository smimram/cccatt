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
	@dune build
	less ../_build/default/src/parser.conflicts
