# libstarlark
Starlark parser and tooling

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
