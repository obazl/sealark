# Sealark - tools for Starlark parsing and editing
Sealark is a collection of tools for working with Starlark code,
written in C (get it, C-lark?). `libsealark` is a Starlark parser;
[Sunlark](https://github.com/obazl/sunlark) is a Scheme binding;
[Moonlark](https://github.com/obazl/moonlark) is a Lua binding;

[List of lark species](https://en.wikipedia.org/wiki/List_of_lark_species). Sunlark is actually a [species](https://en.wikipedia.org/wiki/Sun_lark).

**STATUS** Alpha-ish. The parser works, the Lua code works; i.e. it
  can parse a BUILD.bazel file and expose the AST as a Lua table. The
  moonlark and sunlark bindings work too. They include code  to serialize
  the AST, but not much else (much more is planned). **No windows
  support**. Sorry about that, but I don't have a Windows machine.
  It's been tested on MacOS Big Sur and Linux (Debian Stretch).

**NOTE** The main branch does the stuff described below, but
  development occurs on the dev branch, which is what you should use
  if you want to monitor progress or contribute.

**ROADMAP:** the immediate task is to finish the script libraries (Lua
  and Scheme), which will support programmatic editing of the AST.
  First up: given a source file name and list of dependencies, update
  the BUILD.bazel file. Specifically supporting OCaml projects using
  [OBazl](https://obazl.github.io/docs_obazl/); this is the use case
  that motivated development of moonlark. Also on the to-do list: more
  detailed documentation. A long-term goal is to support editing
  capabilities matching or exceeding those of
  [Buildozer](https://github.com/bazelbuild/buildtools/blob/master/buildozer/README.md).

## quickstart

**WARNING** the first time you run one of the tools it may take a while to build everything (e.g. re2c takes a while to compile).

Sysdeps: libsealark builds re2c, which depends on make, autogen,
autoconf, and autogen.

Add the following to `WORKSPACE.bazel`:

```
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository")
git_repository(
    name = "sealark",
    remote = "https://github.com/obazl/sealark",
    branch = "dev",
)
http_archive(  ## needed to build re2c, which libsealark needs
    name = "rules_foreign_cc",
    sha256 = "e14a159c452a68a97a7c59fa458033cc91edb8224516295b047a95555140af5f",
    strip_prefix = "rules_foreign_cc-0.4.0",
    url = "https://github.com/bazelbuild/rules_foreign_cc/archive/0.4.0.tar.gz",
)
load("@rules_foreign_cc//foreign_cc:repositories.bzl", "rules_foreign_cc_dependencies")
rules_foreign_cc_dependencies(register_built_tools=False)

```

### parse a build file

**NOTE**: The sample code below uses `moonlark`; the UI for `sunlark` is the same, just substitute the former for the latter in the code. It also assumes that you are using Sealark as an external repo. If you have cloned the Sealark repo and are running from its root directory, just drop `@sealark`, e.g. instead of `$ bazel run @sealark//moonlark:edit`, run `$bazel run moonlark:edit`.

The system Lua code for moonlark is in `moonlark/lua`; user code goes in `.moonlark.d`. For sunlark, the system Scheme code is in `sunlark/scm`, and user code goes in `./sunlark.d`.

```
$ mkdir tmp  # for now, this is where serialized output is written
$ bazel run @sealark//moonlark:edit -- -f lib/BUILD.bazel
INFO: Analyzed target //moonlark:edit (0 packages loaded, 0 targets configured).
INFO: Found 1 target...
Target //moonlark:edit up-to-date:
  bazel-bin/moonlark/edit
INFO: Elapsed time: 0.123s, Critical Path: 0.00s
INFO: 1 process: 1 internal.
INFO: Build completed successfully, 1 total action
INFO: Build completed successfully, 1 total action
08:59:10 WARN  bindings/lua/lbazel.c:28: WARNING: no user luadir specified; using default: .moonlark.d
08:59:10 INFO  bindings/lua/libmoonlark.c:405: Lua table 'moonlark' not found; creating
@sealark//moonlark/lua/edit.lua: moonlark_handler emitting starlark to tmp/test.BUILD
$ diff tmp/test.BUILD `pwd`/test/data/strings/BUILD.test
$
```

You can run your own lua code instead of the predefined library.
`moonlark:edit` parses the file, converts the resulting C AST to a Lua
table, loads file `edit.lua`, and calls function `moonlark_handler`,
passing it the Lua AST table.

If `.moonlark.d/edit.lua` exists it will override the default
`@sealark//moonlark/lua/edit.lua`.

You can also pass a lua file to use on the command line with `-l`:

```
$ bazel run @sealark//moonlark:edit -- -f lib/BUILD.bazel -l foo.lua
```

The moonlark Lua library (at `@sealark//moonlark/lua`) will still be
on the load path. It contains various Lua files you can use, e.g.
`serialize.lua` and `pprint.lua`.  Load them with e.g. `require "serialize"`.

**WARNING** the moonlark Lua library is still under development and __will__ change.

### repl

```
$ bazel run @sealark//moonlark:repl
... build output ...
Lua 5.4.3  Copyright (C) 1994-2021 Lua.org, PUC-Rio
> for k,v in pairs(moonlark) do
>> print(k,v)
>> end
version	0.1.0
TOK	table: 0x7ff50740dcb0
iTOK	table: 0x7ff50740f070
pTOK	table: 0x7ff50740f0d0
parse_file	function: 0x1063d3ba0
config_bazel	function: 0x1063d3b80
> moonlark.config_bazel()
> ast = moonlark.parse_file("test/data/strings/BUILD.test") # path to some buildfile in your repo
> pp = require "pprint"
> pp(ast)
{
  build_file = 'test/data/strings/BUILD.test',
  col = 0,
  line = 0,
  subnodes = { {
      col = 0,
      line = 0,
      subnodes = { {
          col = 0,
          line = 0,
          subnodes = { {
              col = 0,
              line = 0,
              q = '"',
              qq = 1,
              s = 'hello',
              t = 'TK_STRING',
              type = 79
            } },
          t = 'TK_Expr_List',
          type = 106
... etc ...
```

**IMPORTANT** Bazel awareness is integrated into `moonlark:edit`, but
  not `moonlark:repl`. The latter launches a Lua interpreter with the
  `moonlark` package preloaded, but it does not assume that it has
  been run by Bazel in a Bazel repo. To make it behave like the former
  you must run `moonlark.config_bazel()`. This will put the moonlark
  Lua library on the load path and change directory to the original
  launch directory.

### run tests

```
$ bazel test @sealark//test/unit  ## runs all test suites
$ bazel test @sealark//test/unit:statements  ## runs a single test suite
```

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
stuff. libsealark does one thing, and the design intention is that it
should be easy to combine it with other small, well-defined,
single-purpose tools (e.g.
[codept](https://github.com/Octachron/codept), which analyzes OCaml
dependencies) to build composite tools. Actually `moonlark` is an
example: it combines libsealark with a Lua tool for analyzing the
AST, and another tool for serializing the AST to a build file. All can
be swapped out for alternative implementations.]

## build prerequisites

re2c: autogen, autoconf, libtool, make - the usual suspects.

Lua: we build liblua with readline. On linux, you must install
libreadline-dev, e.g. `sudo apt-get install -y libreadline-dev`.

Unfortunately readline is GPLed. But it is only used with liblua,
so...? Replacing it with a free-license alternative is on the to-do
list.

## development

Docs are a WiP; see also [devguide](doc/devguide.md).

### libsealark

Target: `//src:starlark`

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
`moonlark`, a Lua module, which also exposes the parsed AST as a Lua
table. AST manipulation and serialization can then be implemented in
Lua code. The idea is that this will make customization much easier
(since Lua is much simpler than C), thus enabling tool makers to build
a variety of tools on top of moonlark/libsealark. Default
implementations are provided, but the user can easily supply
alternatives.

### lua

#### lua bindings

`moonlark` packages libsealark as a lua module, allowing the parser
to be run from a lua program. I.e. it extends Lua.

Target `//moonlark:repl` is the lua application augmented by moonlark.
Running `$ bazel run moonlark:repl` will launch a lua repl with
moonlark preloaded.

Target `//moonlark:edit` is a C application that runs the `libsealark`
routine `starlark_parse_file`, converts the resulting AST to a Lua
table, and invokes a user-supplied Lua function named `handler`.

#### repl

The lua module is called 'moonlark'.

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
`moonlark.parse_file` returns the AST. You need to run
`moonlark.config_bazel` if you want to use the moonlark Lua library.

```
> moonlark.config_bazel()
> serpent = require "serpent"
> moonlark.parse_file("path/to/BUILD.bazel")
> print(serpent.block(bazel.build))
```

#### callback

To have libsealark parse a file using `moonlark:edit` and invoke your
own lua code (a callback) on the AST:

* your lua code goes in `<projroot>/.moonlark.d/edit.lua` (use the code here as an example)
* the callback routine must be a function taking one arg (an AST table) named `moonlark_handler`
* run:

```
$ bazel run moonlark:edit -- -f path/to/BUILD.bazel
```

Moonlark will parse the file (using the C libsealark library) and
convert the AST to a Lua table.

The following lua modules are available for working with the AST:

* `edit.lua` - default callback for moonlark:edit
* `serialize.lua` - for writing the AST to a file as Starlark code
* `pprint.lua` - pretty printing (i.e. serializing to Lua code)

To use them put something like the following in your Lua code:

```
s = require "serialize"
pp = require "pprint"
```

See moonlark/lua for examples.

## testing

For some of the unit tests debug flags must be set. For example,
`test/unit/sunlark:vectors` will fail unless flag
`--//sealark:yydebug=vectors` is passed. The flags are defined in
`//:BUILD.bzl`, `//:BUILD.bazel`, and `//sealark/BUILD.bazel`; see
`bzl/user.bazelrc` for use example.

### Unit tests

```
$ bazel test test/unit   # run all tests
# test suites are targets within test/unit
$ bazel test test/unit:expressions
$ bazel test test/unit:strings
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

Tests in `test/sys` are still under construction.

test:lua_file does not work currently.

### misc

starlark grammar: https://github.com/bazelbuild/starlark/blob/master/spec.md#grammar-reference

bazelbuild buildfile parser stuff:  https://github.com/bazelbuild/buildtools/tree/master/build

lexer (Golang impl):  https://github.com/bazelbuild/buildtools/blob/master/build/lex.go

