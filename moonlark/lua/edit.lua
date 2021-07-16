-- edit.lua

print("hello from @moonlark//moonlark/lua/edit.lua!!!")

function emit_starlark(ast, outfile)
   local serialize = require "serialize"
   serialize.emit_starlark(ast, outfile)
end


function ast_handler(buildfile_ast)
   -- print("@moonlark//moonlark/lua/edit.lua: moonlark_handler")
   -- print("@moonlark//moonlark/lua/edit.lua: moonlark_handler emitting starlark to tmp/test.BUILD")
   -- emit_starlark(buildfile_ast, "tmp/test.BUILD")

   pp = require "pprint"
   pp(buildfile_ast)
end
