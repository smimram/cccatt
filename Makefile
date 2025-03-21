all:
	@dune build

install:
	@dune install

clean:
	@dune clean

test:
	$(MAKE) -C tests $@
	$(MAKE) -C examples $@
	@dune runtest
