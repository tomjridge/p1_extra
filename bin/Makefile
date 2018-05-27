SHELL:=bash

all:
	ocamlfind ocamlopt -w -8 -package p1_extra -linkpkg test.ml

clean:
	rm -f *.{cmi,cmo,cmx,o,a,cmxa,cma} a.out generated_example.ml
