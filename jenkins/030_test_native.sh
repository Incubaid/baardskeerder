#!/bin/bash -eux
export ROOT=$(pwd)/../ROOT/
export LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:${ROOT}/OCAML/lib
cd src
./bsmgr.native --hudson
