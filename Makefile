default: build 
      utop

build: 
  ocamlbuild -use-ocamlfind main.byte

test:
  ocamlbuild -use-menhir -package oUnit -use-ocamlfind test.byte && ./test.byte

clean:
  ocamlbuild -clean


calc:
  ocamlbuild -use-menhir main.byte && ./main.byte
