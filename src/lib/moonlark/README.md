# lua extension module

Allows lua to be used to extend libstarlark.  The lib will call user-provided lua code to process the AST.


Setup:

1. config_lua("starlark.lua")
2. init_lua()

## converting AST from C to Lua data

See `tools_obazl/src/bindings/lua/obazl_dune_lua.c`

E.g. `stanzas_table` is a C/Lua API, takes a stanzas list arg on the
stack and creates a lua table for it. Designed as a lua fn, but only
called by the C code (for dune parsing).

TODO: make this a C API, not a Lua API. So libstarlark can call it
directly. The Lua Binding can then have a Lua/C API that calls it.

I.e. the internal C APIs that convert AST to lua need not be directly
exposed in the Lua API; a user Lua app will presumably never need to
call them directly. They're just C code that creates Lua tables in the
(configured) master table, of which we will have one per BUILD.bazel
file.  (Let's call that table "build_bazel"?)

So the process is:

1. parse the BUILD.bazel file (C code)

2. initialize Lua

3. create a global table for the file, called 'build_bazel'

4. crawl the AST, creating entries in the 'build_bazel' table

5. invoke the user-provided handler, passing the Lua 'build_bazel'
   table (which is the Lua version of the AST)

6. User's handler fn updates the (Lua) AST, then serializes it
