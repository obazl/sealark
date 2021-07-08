-- edit.lua

print("hello from .moonlark.d/edit.lua")

print(bazel)

for k,v in pairs(bazel) do
   print(k,v)
end

function init(buildfile_ast)
   print(".moonlark.d/edit.lua: init")
   for k,v in pairs(buildfile_ast) do
      print(k,v)
   end
   print("bazel:")
   print(bazel)
   for k,v in pairs(bazel) do
      print(k,v)
   end
   print("returning")
end
