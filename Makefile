SHELL:=bash

build: 
	dune build @install
	dune build bin/test_combinators.exe  # FIXME and more; how to build all exes?


install:
	dune install

clean:
	dune clean
	rm -f *.install
