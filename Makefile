all:
	ocamlbuild tiger_main.native

clean:
	ocamlbuild -clean

.PHONY: all clean
