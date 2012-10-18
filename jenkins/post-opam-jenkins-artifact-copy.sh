# Will set OPAM
. ROOT/OPAM/envuser/defaults.sh
. ROOT/OPAM/envuser/activate.sh

ROOT/OPAM/envuser/post-artifact-copy

eval `$OPAM config -env`

cd src
make
#./bsmgr.byte --hudson
./bsmgr.native --hudson

# Reinstall

make uninstall
make install
