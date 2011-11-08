#!/bin/bash

set -e

WORKSPACE=/home/incubaid/nicolas/baardskeerder_benchmark
TREE=${WORKSPACE}/baardskeerder
CONFIG=${WORKSPACE}/codespeed.config
SCRIPT=${TREE}/scripts/codespeed.py
BRANCH=refs/heads/master
LOCKFILE=/tmp/baardskeerder_codespeed.lock

set +e
fuser ${LOCKFILE} > /dev/null 2>&1
LOCKED=$?
set -e
if [ $LOCKED -eq 0 ]; then
    echo "File locked"
    exit
fi

pushd ${TREE}
START_REV=`git show-ref ${BRANCH} | cut -d' ' -f1`
git pull
NEW_REV=`git show-ref ${BRANCH} | cut -d' ' -f1`
popd

if [ x${START_REV} = x${NEW_REV} ];
then
    exit
fi

pushd ${TREE}/src
ocamlbuild -use-ocamlfind benchmark.native
popd

python ${SCRIPT} ${LOCKFILE} ${CONFIG} ${NEW_REV}
exit $?
