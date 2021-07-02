# BUILD.bazel parsing


testing:

```
$ bazel run src/lib/obazl_starlark:test_parse --  -f `pwd`/data/test.BUILD.bazel
```

similar for `test_lex`


starlark grammar: https://github.com/bazelbuild/starlark/blob/master/spec.md#grammar-reference

bazelbuild buildfile parser stuff:  https://github.com/bazelbuild/buildtools/tree/master/build

lexer (Golang impl):  https://github.com/bazelbuild/buildtools/blob/master/build/lex.go


### misc

generate tokens.h from tokens.txt (list of TOKEN names)

rg -N . src/lib/obazl_starlark/tokens.txt | sort | uniq > toks.h
nl -w 2 -n rz -s ' ' toks.h > toksnl.h

paste -d ' ' toksnl.h toks.h | cut -d ' ' -f2,3 | sed 's/^/#define /' > src/lib/obazl/bazel/tokens.h

generate tokens.c, with token_name lookup table

sed 's/\(.*\)/[\1] = "\1",/' toks.h > src/lib/obazl_starlark/tokens.c




