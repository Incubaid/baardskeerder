#!/bin/bash -eux

export PATH=/home/qbase/ROOT/OCAML/bin:$PATH
export LD_LIBRARY_PATH=/usr/lib:/usr/local/lib:/home/qbase/ROOT/OCAML/lib
cd src
./bsmgr.native --hudson
