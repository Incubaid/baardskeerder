#!/bin/bash -eux
export ROOT=$(pwd)/../ROOT/
export PATH=${ROOT}/OCAML/bin:$PATH
export LD_LIBRARY_PATH=${ROOT}/OCAML/lib:/usr/lib:/usr/local/lib
cd src
which ocamlfind
make uninstall
make install
