-- edit.lua

print("hello from .moonlark.d/edit.lua")

print(bazel)

for k,v in pairs(moonlark) do
   print(k,v)
end

function init(buildfile_ast)
   print(".moonlark.d/edit.lua: init")
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
