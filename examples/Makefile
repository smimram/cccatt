test:
	@dune runtest -f

bug:
	../cccatt bug.cccatt

profile:
	@dune build
	perf record --call-graph=dwarf -- ../_build/default/src/cccatt.exe bicategory-cartesian.cccatt
	perf report
