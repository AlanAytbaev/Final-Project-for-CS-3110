MODULES=lexer parser arithmetic 
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind
PKGS=unix.oUnit

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	ocamlbuild -use-ocamlfind test.byte && ./test.byte

clean:
	ocamlbuild -clean

calc:
	ocamlbuild main.byte && ./main.byte
