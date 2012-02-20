#!/bin/bash -eux

if test -f /usr/lib/x86_64-linux-gnu/libuuid.so; then
    export LD_LIBRARY_PATH=/usr/lib/x86_64-linux-gnu:$LD_LIBRARY_PATH
fi
export OCAMLPATH=`pwd`/_deps/inst

echo "LD_LIBRARY_PATH=$LD_LIBRARY_PATH"
echo "OCAMLPATH=$OCAMLPATH"

which ocamlbuild
ocaml -version
cd src && ocamlbuild -use-ocamlfind bsmgr.byte bsmgr.native
