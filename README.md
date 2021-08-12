# Sealark - tools for Starlark parsing and editing
Sealark is a collection of tools for working with Starlark code,
written in C (get it, C-lark?). `libsealark` is a Starlark parser;
[Sunlark](https://github.com/obazl/sunlark) is a Scheme binding;
[Moonlark](https://github.com/obazl/moonlark) is a Lua binding;

[List of lark species](https://en.wikipedia.org/wiki/List_of_lark_species). Sunlark is actually a [species](https://en.wikipedia.org/wiki/Sun_lark).

**STATUS** Beta-ish. Used by
[Sunlark](https://github.com/obazl/sunlark); documentation is on the
way.

## testing

```
$ bazel run test:lex_file -- -f path/to/BUILD
```

```
$ bazel run test:parse_file -- -f path/to/BUILD
```

Unit tests are currently in [Sunlark](https://github.com/obazl/sunlark/tree/main/test/unit/sunlark).
