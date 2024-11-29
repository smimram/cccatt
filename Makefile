all:
	@dune build

install:
	@dune install

clean:
	@dune clean

test:
	@dune runtest
	$(MAKE) -C tests $@
