SHELL:=bash

all: test.native test_lambda_calc.native

%.native: %.ml
	ocamlfind ocamlopt -w -8 -package p1_extra -linkpkg -o $@ $*.ml

clean:
	rm -f *.{cmi,cmo,cmx,o,a,cmxa,cma} a.out generated_example.ml *.native
