# development

**CAVEAT** Work-in-Progress. May not match source code.

## C

### makeheaders

In many cases we need to run makeheaders against files in different
packages. We expose them using `exports_files`. It is not sufficient
to depend on the other library; the other library's exposed headers
will be available, but the depending file, say `edit.c`, contains only
`edit.h`, produced by makeheaders. So the other package's __source__
files must be included as input to the makeheaders command.

Note that this does not affect the Bazel dependency graph, so circular
dependencies will not be introduced.

## lua

WARNING: outdated. lua support is moribund. 

Neither libsunlark nor libmoonlark depend on the Bazel environment.
It's up to the app to deal with it.

`//moonlark:edit` is a C application that depends on libmoonlark but
not the moonlark Lua package. It is also Bazel-aware: it elaborates
the Lua load path to include `//moonlark/lua` and the user Lua
directory, which defaults to `//.moonlark.d`. It also accounts for the
"runfiles" directory, which becomes the current directory when it is
launched as `$ bazel run moonlark:edit`. It will set the Lua load path
accordingly (which will include a hidden directory set up by Bazel)
and then change directory to the original launch directory (which
should be the root directory of a Bazel project.) Finally, it creates
a global `bazel` table that contains useful subtables (e.g.
`bazel.config` contains config info, `bazel.TOK` is a table of grammar
constants, etc.; see X for details.)

`//moonlark:repl` is a Lua interpreter with integrated moonlark
support. The `moonlark` Lua package is preloaded. It is not
Bazel-aware by default, but it comes with Lua code
(`//moonlark/lua/repl.lua`) that can be used to configure it the way
`//moonlark:edit` is configured. That code can be loaded by passing a
command line argument, e.g. `$ bazel run moonlark:repl -- -i
moonlark/lua/repl.lua`.

[Alternative: the `moonlark` package exposes `config_bazel`, which
will do same in C code.]

### initialization

#### C apps

Two options: with or without preconfigured Bazel integration.

##### With preconfigured Bazel support

Startup code must set the load path and create the `bazel` table
and subtables. It then calls a user-provided routine.

Example: `moonlark/edit.c`.

1. Query the environment to compute the load paths and set the working directory.

When `bazel run` is used to launch an executable, Bazel switches the
    current directory to a hidden "runfiles" directory in the sandbox.
    This means that opening relative filepaths may fail, unless you
    have configured things just so. Bazel exposes the original launch
    directory via the environment variables
    `BUILD_WORKSPACE_DIRECTORY` and `BUILD_WORKING_DIRECTORY`; see
    [Running
    executables](https://docs.bazel.build/versions/main/user-manual.html#run)
    for more information.

This is an issue for two reasons. One is that you may want to
    `require` the lua files in `moonlark/lua` or `.moonlark.d`. The
    other is that you may want your Lua to read or write files. In the
    former case, you need to make sure the directories are on the Lua
    load path; in the latter case, you want to be able to use paths
    that are relative to the original launch directory.

Furthermore, elaborating the Lua load path by simply adding a
    directory relative to the original launch directory (e.g.
    `moonlark/lua`) will not work when your repo is used as an
    external resource in another project. In that case, your project's
    code will only be available in a directory located in Bazel's
    hidden work area; you can see something of this by running `$
    bazel info`.

This is where "runfiles" enter the picture. If you know ahead of
    time that certain files may be accessed, you can designate them as
    "runfiles" by using the `data` attribute of the build rule. Bazel
    will use links to add such files to runfiles directory. That way
    your code can still reference such files using relative paths. For
    example, `data = ["//moonlark/lua:edit.lua"]` tells Bazel to add
    `moonlark/lua/edit.lua` to the runfiles directory.

This is the strategy used by `//moonlark:edit` (and `//moonlark:repl`) - see the targets in `moonlark/BUILD.bazel`.

However there is are a few complications. By making `moonlark/lua/edit.lua` a runfile, we could add `moonlark/lua` to
    the Lua load path, and then `require("edit")` would work.
    Unfortunately it would only work when run from this project root.
    If moonlark were added as an external repo to another project, it
    would fail, because in that case Bazel inserts directory segments
    "external/<reponame>", so the Lua file would be available as
    `external/moonlark/moonlark/lua` (where the first 'moonlark' is
    the repo name). Secondly, since the current working directory
    would be the runfiles directory, accessing files in the original
    launch directory would require absolute paths, which is tedious
    and error-prone.

To make a long story a bit shorter: what we need to do is the
    following (not necessarily in this order):

A. Get the current working directory at startup (`getcwd()`); this
    will be the runfiles directory.

B. Find and read the runfiles MANIFEST file; it will be in the
    parent directory. It contains mappings from runfiles to their
    locations; for example:

    moonlark/moonlark/lua/edit.lua /private/var/tmp/_bazel_gar/68...c637c/external/moonlark/moonlark/lua/edit.lua

(We designated `moonlark/lua/edit.lua` as a runfile; the first
    'moonlark' here is the directory name of the project root, which Bazel prefixes to get `moonlark/moonlark/lua/edit.lua`.)

C. Start up the Lua runtime - `luaL_newstate()`

D. Add the (absolute path of the) runfiles directory to the Lua load
    path; in this example that would be

        /private/var/tmp/_bazel_gar/68...c637c/external/moonlark/moonlark/lua

By using an absolute path, we ensure that files in the directory
    can be `require`d no matter what the current working directory is.

E. Add the user lua dir to the path. This must be relative to the
original launch dir (i.e. proj root). We cannot designate user files
as runfiles, so we need to check at runtime to see if `.moonlark.d`
exists, and if so add it to the path.

FIXME: support a config option/file allowing user to set custom lua dir

F. Read env. var. `BUILD_WORKING_DIRECTORY`, which should be the
    original launch directory; then switch to it (`chdir(wd)`).

2.  Create `bazel` table and subtables

The `bazel` table contains fields:

* `config`: list of configuration settings, e.g. project_root,
  runfiles_root, etc.

* TOK - table mapping token name strings to codes, e.g. `bazel.TOK.LPAREN => 54`

* iTOK - table mapping token codes to string names, e.g. `bazel.iTOK[54] => "LPAREN"`

* pTOK - table mapping token codes to printable strings, e.g.
  `pTOK[bazel.TOK.LPAREN]` => "("

#### Without preconfigured Bazel support:

## Troubleshooting

`/bin/bash: line 9: bindings/scheme/sunlark_properties.c: Permission denied`

This is cause by a syntax error in a :mkhdrs target:

```
    cmd = "\n".join([
        "SRC1=$(location libsunlark.c)",
        "SRCDIR1=`dirname $$SRC1`",
        "$(location //vendored/makeheaders) \\",
        ...blah blah...
        "    $(location ast_node_s7.c) \\",
        "    $(location ast_nodelist_s7.c)",  <<===== missing // before "
        "    $(location sunlark_properties.c)",
        "cp $${SRCDIR1}/*.h $(@D)",
    ]),
```

WARNING: //vendored has been removed, see MODULE.bazel

## CAVEATS

* Bazel may behave a little erratically when used as a launcher. The
  RUNFILES env. vars may or may not be defined, depending on whether
  run cmd was 'run' or 'test', and whether 'run' is used to launch a
  cc_binary or a cc_test target. Also, the runtime environments for
  the 'test' and 'run' commands are different.


### misc

obsolete:

generate tokens.h from tokens.txt (list of TOKEN names)

rg -N . src/lib/obazl_starlark/tokens.txt | sort | uniq > toks.h
nl -w 2 -n rz -s ' ' toks.h > toksnl.h

paste -d ' ' toksnl.h toks.h | cut -d ' ' -f2,3 | sed 's/^/#define /' > src/lib/obazl/bazel/tokens.h

generate tokens.c, with token_name lookup table

sed 's/\(.*\)/[\1] = "\1",/' toks.h > src/lib/obazl_starlark/tokens.c


