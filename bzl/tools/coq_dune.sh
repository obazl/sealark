#!/bin/bash

COQDEP="bazel-out/darwin-fastbuild/tools/coqdep"

# %{bin:coqdep}

bash "$COQDEP -dyndep both -noglob -boot \
     -R theories Coq \
     -Q user-contrib/Ltac2 Ltac2 \
     -I user-contrib/Ltac2 \
     `find theories user-contrib -type f -name *.v`"

##     `find plugins/ -maxdepth 1 -mindepth 1 -type d -printf '-I %p '` \
