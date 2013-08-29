all:
	ocamlbuild main.native

clean:
	ocamlbuild -clean

.PHONY: all clean
