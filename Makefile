all test:
	@dune runtest
	make -C test $@

.PHONY: test
