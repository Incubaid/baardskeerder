#!/bin/bash -eux

export PATH=/home/qbase/ROOT/OCAML/bin:$PATH
export LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/home/qbase/ROOT/OCAML/lib
which ocamlbuild
ocaml -version

rm -rf _deps/src/quickcheck
rm -rf _deps/inst/quickcheck

mkdir -p _deps/src _deps/inst

cd _deps/src
git clone git://github.com/Incubaid/ocaml-quickcheck.git quickcheck
cd quickcheck
make
OCAMLFIND_DESTDIR=`pwd`/../../inst make install
