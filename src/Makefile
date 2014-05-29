DEPENDENCIES=sha,bitstring,bitstring.syntax
SYNTAX=bitstring.syntax

all: main.ml
	ocamlfind ocamlc -package $(DEPENDENCIES) -syntax $(SYNTAX) -linkpkg $< -o bitcaml

