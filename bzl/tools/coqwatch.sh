#!/bin/sh

## PROOF OF CONCEPT ONLY!

## This script is designed to be run by https://github.com/emcrisostomo/fswatch
## When a watched file changes, this script will run coqdep to reanalyze its dependencies,
## and then run genbuildfiles.joke to generate new BUILD.bazel files from the coqdep output.

## fswatch -0xr theories | xargs -0 -n 1 -I {} ./bzl/tools/coqdeps.sh {}

FPATH="${1%% *}"
echo "FPATH: $FPATH"

TGT="${FPATH#`pwd`/}"
TGTDIR=`dirname $TGT`

echo "TGTDIR: $TGTDIR"

# echo $1

if  [[ "$1" == *"BUILD.bazel"* ]]
then
    echo "SKIPPING BUILD.bazel"
elif  [[ "$1" == *" Removed"* ]]
then
    if [[ "$1" == *" IsFile"* ]]
    then
       echo "REMOVED: $TGT"
    else [[ "$1" == *" IsFile"* ]]
       echo "IGNORING1: $TGT"
    fi
elif [[ "$1" == *" Updated"* ]]
then
    echo "CHANGED: $TGT"
    mkdir -p ./.obazl.d/$TGTDIR;
    ./bazel-bin/tools/coqdep -f bzl/_CoqProject $TGTDIR -coqlib bazel-out/darwin-fastbuild/bin > .obazl.d/$TGTDIR".deps"
    joker bzl/tools/genbuildfiles.joke --coqdeps .obazl.d/$TGTDIR.deps
elif  [[ "$1" == *" Created IsFile"* ]]
then
    echo "CREATED: $TGT"
else
    echo "IGNORING2: $TGT"
fi

#    ./bazel-bin/tools/coqdep -f bzl/_CoqProject $TGT -coqlib bazel-out/darwin-fastbuild/bin > .obazl.d/$TGT".deps"

# ./bazel-bin/tools/coqdep -f bzl/_CoqProject theories/Arith/ -coqlib bazel-out/darwin-fastbuild/bin > .obazl.d/Arith.deps
