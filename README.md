# libstarlark
Starlark parser and tooling

starlark grammar: https://github.com/bazelbuild/starlark/blob/master/spec.md#grammar-reference

bazelbuild buildfile parser stuff:  https://github.com/bazelbuild/buildtools/tree/master/build

lexer (Golang impl):  https://github.com/bazelbuild/buildtools/blob/master/build/lex.go

## testing

Unit tests:

```
$ bazel test test
$ bazel test:expressions
etc.
```

Ad-hoc testing:

Run the lexer only, dumping result to stdout:

```
$ bazel run src:test_lex -- -f `pwd`/data/test.lex.BUILD.bazel
```


Run the parser against a file:

```
$ bazel run src:test_parse -- -f `pwd`/data/bazel/examples/cpp/BUILD
$ diff data/bazel/examples/cpp/BUILD test.BUILD.bazel
```

This dumps trace/debug messages to stdout and serializes the parsed
AST to `test.BUILD.bazel` in the current directory.

### misc

generate tokens.h from tokens.txt (list of TOKEN names)

rg -N . src/lib/obazl_starlark/tokens.txt | sort | uniq > toks.h
nl -w 2 -n rz -s ' ' toks.h > toksnl.h

paste -d ' ' toksnl.h toks.h | cut -d ' ' -f2,3 | sed 's/^/#define /' > src/lib/obazl/bazel/tokens.h

generate tokens.c, with token_name lookup table

sed 's/\(.*\)/[\1] = "\1",/' toks.h > src/lib/obazl_starlark/tokens.c


