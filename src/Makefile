OCAML_FIND ?= ocamlfind

build:
	ocamlbuild -use-ocamlfind test.native baardskeerder.cma baardskeerder.cmxa baardskeerder.a

clean:
	ocamlbuild -use-ocamlfind -clean
install:
	$(OCAML_FIND) install baardskeerder META \
	  _build/baardskeerder.cma \
	  _build/baardskeerder.cmxa \
	  _build/baardskeerder.mli \
	  _build/baardskeerder.cmi \
          _build/posix.o \
	  _build/libcrc32c.a \
	  _build/baardskeerder.a


uninstall:
	$(OCAML_FIND) remove baardskeerder


default: build
