OCAML_FIND ?= ocamlfind

build:
	ocamlbuild -use-ocamlfind bsmgr.native baardskeerder.cma baardskeerder.cmxa baardskeerder.a bs_internal.cma bs_internal.cmxa

clean:
	ocamlbuild -use-ocamlfind -clean
install:
	$(OCAML_FIND) install baardskeerder META \
	  _build/baardskeerder.cma \
	  _build/baardskeerder.cmxa \
	  _build/bs_internal.cma \
	  _build/bs_internal.cmi \
	  _build/bs_internal.cmxa \
	  _build/baardskeerder.mli \
	  _build/baardskeerder.cmi \
	  _build/libbaardskeerder_c.a \
	  _build/baardskeerder.a


uninstall:
	$(OCAML_FIND) remove baardskeerder

coverage:
	ocamlbuild -use-ocamlfind \
	-tag 'package(bisect)' \
	-tag 'syntax(camlp4o -no_quot)' \
	-tag 'syntax(bisect_pp)' \
	test.byte

default: build
