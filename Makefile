all:
	@dune build

install:
	@dune install

clean:
	@dune clean

test:
	@dune runtest
	make -C test $@

.PHONY: test
