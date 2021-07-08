-- edit.lua

print("hello from @moonlark//moonlark/lua/edit.lua")

print(bazel)

for k,v in pairs(moonlark) do
   print(k,v)
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
   print("returning")
end
