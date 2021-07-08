-- edit.lua

print("hello from @moonlark//moonlark/lua/edit.lua")

function emit_starlark(ast, outfile)
   local serialize = require "serialize"
   serialize.emit_starlark(ast, outfile)
end


function moonlark_handler(buildfile_ast)
   print("@moonlark//moonlark/lua/edit.lua: moonlark_handler")
   print("ast for " .. buildfile_ast.build_file .. ":")
   for k,v in pairs(buildfile_ast) do
      print(k,v)
   end
   print("moonlark table:")
   print(moonlark)
   for k,v in pairs(moonlark) do
      print(k,v)
   end

   emit_starlark(buildfile_ast, "tmp/test.BUILD")

   -- serpent = require "serpent"
   -- print(serpent.dump(buildfile_ast))

   local pprint = require('pprint')
   pprint(buildfile_ast)

end
