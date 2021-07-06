# moonlark/libstarlark
Starlark parser in C11 and Lua module

**STATUS** Pre-pre-alpha. The parser works, the Lua code works; i.e. it can parse a BUILD.bazel file and expose the AST as a Lua table.  The moonlark repl works too. But the (Lua) code to edit the AST and emit Starlark code is still under construction.  Also: testing has been limited to this repo; iow it has not been tested as an external repo. Some of the paths will probably need adjusting. Also, only tested on MacOS. Linux will be supported Real Soon Now.

**NOTE** The main branch does the stuff described below, but will not be updated until a beta version is reached. Development now occurs on the dev branch, which is what you should use if you want to monitor progress or contribute.

starlark grammar: https://github.com/bazelbuild/starlark/blob/master/spec.md#grammar-reference

bazelbuild buildfile parser stuff:  https://github.com/bazelbuild/buildtools/tree/master/build

lexer (Golang impl):  https://github.com/bazelbuild/buildtools/blob/master/build/lex.go

## motivation

The motivating use case was the need to automatically update
BUILD.bazel files for OCaml/Coq projects. Dependency management is
somewhat complicated in OCaml, and manual updating of dependencies in
BUILD files is tedious and error prone. But all the information needed
to update the BUILD files is available in the source code; all that is
needed is tooling to determine the dependencies and then update the
files. A dependency analysis tool is already available
([Codept](https://github.com/Octachron/codept)), but no suitable tool
for programmatically editing BUILD files was available. I considered
[Gazelle](https://github.com/bazelbuild/bazel-gazelle), but decided
that learning how extend Gazelle to support the OCaml use case would
probably take about as much effort as just writing a Starlark parser.
In addition, depending on Gazelle means depending on Go, and I prefer
to avoid forcing potential users to deal with that, even if Bazel does
make it mostly invisible. The path of least resistence is C, since
just about every system already has a C toolchain, and just about
every language can integrate a C library with reasonable effort.

[TODO: more detailed comparison with Gazelle. Gazelle is a powerful
tool, why do we need another one? Short answer: The Unix Way - small,
well-defined, single-purpose tools. Gazelle does a whole bunch of
stuff. libstarlark does one thing, and the design intention is that it
should be easy to combine it with other small, well-defined,
single-purpose tools (e.g.
[codept](https://github.com/Octachron/codept), which analyzes OCaml
dependencies) to build composite tools. Actually `moonlark` is an
example: it combines libstarlark with a Lua tool for analyzing the
AST, and another tool for serializing the AST to a build file. All can
be swapped out for alternative implementations.]

## libstarlark

Target: `//src:starlark`

`libstarlark` is a C11 library that contains routines to parse files
and strings of Starlark code, producing a simple AST. It also contains
some serialization routines to write the AST to a string. The result
can be compared to the original input. (The goal is a 100% match,
including whitespace and comments).

`libstarlark` uses [re2c](https://re2c.org/) for lexing,
[lemon](https://www.sqlite.org/cgi/src/doc/trunk/doc/lemon.html) for
parsing, and [uthash](https://troydhanson.github.io/uthash/) for C
data structures.

Currently `libstarlark` does not contain a public API for manipulating
the AST. A developer could easily implement such routines, however,
since the AST is pretty simple, and it uses `utarray` and `utstring`
from the [UTHash](https://troydhanson.github.io/uthash/) library.

Instead, the parsing routines of `libstarlark` are exposed in
`moonlark`, a Lua module, which also exposes the parsed AST as a Lua
table. AST manipulation and serialization can then be implemented in
Lua code. The idea is that this will make customization much easier
(since Lua is much simpler than C), thus enabling tool makers to build
a variety of tools on top of moonlark/libstarlark. Default
implementations are provided, but the user can easily supply
alternatives.

## lua

### lua bindings

`moonlark` ('lua' is Portugese for 'moon') packages libstarlark as a
lua module, allowing the parser to be run from a lua program. I.e.
it extends Lua.

Target `//moonlark:repl` is the lua application augmented by moonlark.
Running `$ bazel run moonlark:repl` will launch a lua repl with
moonlark preloaded.

By contrast, `extensions/lua` contains code that allows lua to serve
as a libstarlark extension. The parser delegates processing to a
user-provided lua helper program.

Target `//moonlark:edit` is a C application that runs the `libstarlark`
routine `starlark_parse_file`, converts the resulting AST to a Lua
table, and invokes a user-supplied Lua function named `handler`.

### repl

The lua module is called 'moonlark' ('lua' is 'moon' in Portugese).

Start a lua repl, with moonlark preloaded:

```
$ bazel run moonlark:repl
```

To run lua code at startup, write it to a file in `~/.moonlark.d` and
pass it with `-i` (note the double-hyphen `--`.) :

```
$ bazel run moonlark:repl -- -i `pwd`/.moonlark.d/mytest.lua
```

At the repl you can parse a file and serialize the result.
`moonlark.parse_file` puts the AST in global table `bazel.build`:

```
> serpent = require "serpent"
> moonlark.parse_file("path/to/BUILD.bazel")
> print(serpent.block(bazel.build))
```

### callback

To have libstarlark parse a file and invoke your own lua code (a callback)
on the AST:

* your lua code goes in `<projroot>/.moonlark.d/handler.lua` (use the code here as an example)
* run:

```
$ bazel run moonlark:edit -- -f path/to/BUILD.bazel
```

Moonlark will parse the file (using the C libstarlark library) and
convert the AST to a Lua table, which will be available in global
table `bazel.build`. So if the build file is `foo/bar/BUILD.bazel`,
the AST will be in `bazel.build["foo/bar/BUILD.baze"]`.

The following lua modules are available for working with the AST:

* `moonlark_edit` - for manipulating the AST
* `moonlark_serialize` - for writing the AST to a file as Starlark code

To use them put something like the following in your Lua code:

```
mled = require "moonlark_edit"
mls = require "moonlark_serialize"
...
mls.emit(bazel.build["foo/bar/BUILD.bazel"], "foo/bar/BUILD.test.bazel")
...
```

## testing

### Unit tests

```
$ bazel test/unit   # run all tests
# test suites are targets within test/unit
$ bazel test/unit:expressions
$ bazel test/unit:strings
etc.
```

### Ad-hoc testing

Lex a file, dumping result to stdout:

```
$ bazel run test:lex_file -- -f `pwd`/data/test.lex.BUILD
```


Parse a file:

```
$ bazel run test:parse_file -- -f `pwd`/data/bazel/examples/cpp/BUILD
```

This dumps trace/debug messages to stdout and serializes the parsed
AST to a temporary file. It then compares the output to the input.

See also `$ bazel run test/unit:roundtrip`

### misc

obsolete:

generate tokens.h from tokens.txt (list of TOKEN names)

rg -N . src/lib/obazl_starlark/tokens.txt | sort | uniq > toks.h
nl -w 2 -n rz -s ' ' toks.h > toksnl.h

paste -d ' ' toksnl.h toks.h | cut -d ' ' -f2,3 | sed 's/^/#define /' > src/lib/obazl/bazel/tokens.h

generate tokens.c, with token_name lookup table

sed 's/\(.*\)/[\1] = "\1",/' toks.h > src/lib/obazl_starlark/tokens.c


