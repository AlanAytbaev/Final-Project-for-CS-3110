MODULES=lexer parser arithmetic main
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -use-menhir
PKGS=unix.oUnit

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

clean:
	ocamlbuild -clean

calc:
	$(OCAMLBUILD) main.byte && ./main.byte
