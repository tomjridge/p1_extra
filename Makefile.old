SHELL:=bash

build: 
	dune build @install
	dune build bin/all.touch

#	dune build bin/test_combinators.exe bin/test_lambda_calc.exe  # FIXME and more; how to build all exes?

run_examples:
	dune exec bin/test_lambda_calc.exe
	dune exec bin/test_combinators.exe

install:
	dune install

clean:
	dune clean
	rm -f *.install
