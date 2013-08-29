all:
	ocamlbuild main.byte

clean:
	ocamlbuild -clean

.PHONY: all clean
