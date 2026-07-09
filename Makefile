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

DIST = /tmp/cccatt.tar.gz

dist:
	-rm $(DIST)
	tar zcvf $(DIST) cccatt *.el *.opam CHANGES.* dune-project LICENSE README.md \
	src/*.ml src/*.mll src/*.mly src/dune \
	examples/dune examples/*.cccatt \
	tests/*.cccatt tests/dune
	rm -rf /tmp/cccatt && mkdir /tmp/cccatt && cd /tmp/cccatt && tar zxvf $(DIST) && dune build && dune test
