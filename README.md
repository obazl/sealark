# Sealark - tools for Starlark parsing and editing
Sealark is a collection of tools for working with Starlark code,
written in C (get it, C-lark?). `libsealark` is a Starlark parser;
[Sunlark](https://github.com/obazl/sunlark) is a Scheme binding;
[Moonlark](https://github.com/obazl/moonlark) is a Lua binding;

[List of lark species](https://en.wikipedia.org/wiki/List_of_lark_species). Sunlark is actually a [species](https://en.wikipedia.org/wiki/Sun_lark).

**STATUS** Beta-ish. Used by
[Sunlark](https://github.com/obazl/sunlark); documentation is on the
way.

## libsealark

`libsealark` is a C11 library that contains routines to parse files
and strings of Starlark code, producing a simple AST. It also contains
some serialization routines to write the AST to a string. The result
can be compared to the original input. (The goal is a 100% match,
including whitespace and comments).

`libsealark` uses [re2c](https://re2c.org/) for lexing,
[lemon](https://www.sqlite.org/cgi/src/doc/trunk/doc/lemon.html) for
parsing, and [uthash](https://troydhanson.github.io/uthash/) for C
data structures. Experienced C programmers will notice there are no
header (.h) files in the source tree. That's because it uses
[Makeheaders](https://fossil-scm.org/home/doc/trunk/src/makeheaders.html#H0009),
which automatically generates one header for each source file,
containing everything it needs (and nothing more). Each BUILD.bazel
file contains a `:mkhdrs` target that runs `makeheaders`. In addition
`sealark/BUILD.bazel` has a `:mkhdrs-export` that generates the
`sealark.h` public API.

Currently `libsealark` does not contain a public API for manipulating
the AST. A developer could easily implement such routines, however,
since the AST is pretty simple, and it uses `utarray` and `utstring`
from the [UTHash](https://troydhanson.github.io/uthash/) library.

Instead, the parsing routines of `libsealark` are exposed in
`sunlark`, a Scheme binding, which also exposes the parsed AST. AST
manipulation and serialization can then be implemented in Scheme code.
The idea is that this will make customization much easier, thus
enabling tool makers to build a variety of tools on top of
sunlark/sealark. Default implementations are provided, but the
user can easily supply alternatives.

A Lua binding, [Moonlark](https://github.com/obazl/moonlark), is
partially implemented.

## testing

```
$ bazel run test:lex_file -- -f path/to/BUILD
```

```
$ bazel run test:parse_file -- -f path/to/BUILD
```

Unit tests are currently in [Sunlark](https://github.com/obazl/sunlark/tree/main/test/unit/sunlark).
