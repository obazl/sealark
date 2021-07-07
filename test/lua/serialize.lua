function emit()
   serialize = require "moonlark_serialize"
   for k,v in pairs(bazel.build) do
      -- print(k,v)
      serialize.test(bazel.build[k])
   end
end



function init()
   print("hello from test/lua/serialize.lua init!")
   emit()
end
