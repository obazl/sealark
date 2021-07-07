-- moonlark_serialize.lua

print("hello from moonlark_serialize.lua")

line = 0
col  = 0

function node_handler(node)
   -- if printable
   if (bazel.pTOK[node.type]) then
      i = line
      while (i < node.line) do
         print()
         line = line + 1
         col = 0
         i = i + 1
      end
      while (col < node.col) do
         io.write(" ")
         col = col + 1
      end
      if (node.type == bazel.TOK.STRING) then
         extra = 0
         if (node.binary) then io.write("b"); extra = extra + 1 end
         if (node.raw) then io.write("r"); extra = extra + 1 end
         if (node.qq == 3) then
            io.write(node.q); io.write(node.q); io.write(node.q)
         else
            io.write(node.q)
         end
         io.write(node.s)
         if (node.qq == 3) then
            io.write(node.q)
            io.write(node.q)
            io.write(node.q)
         else
            io.write(node.q)
         end
         if (node.qq == 3) then
            extra = extra + 6
         else
            extra = extra + 2
         end
         col = col + #(node.s) + extra
      elseif (node.type == bazel.TOK.ID) then
         io.write(node.s)
         col = col + #(node.s)
      elseif (node.type == bazel.TOK.INT) then
         io.write(node.s)
         col = col + #(node.s)
      elseif (node.type == bazel.TOK.FLOAT) then
         io.write(node.s)
         col = col + #(node.s)
      else
         s = bazel.pTOK[node.type]
         io.write(s)
         col = col + #s
      end
   end
   io.flush()
end

-- FIXME: put 'walk' in a different lib ('ast.lua'?)
function walk(t, handlers)
   if (handlers[t.type]) then
      handlers[t.type](t)
   else
      handlers.default(t)
   end
   if (t.subnodes) then
      for k,v in ipairs(t.subnodes) do
         walk(v, handlers)
      end
   end
end

function test(node)
   -- for k,v in pairs(bazel.TOK) do
   --    print(k,v)
   -- end
   print("serialize.test");
   handlers = {}
   handlers.default = node_handler
   walk(node, handlers)
   print()
   io.flush()
end

function emit(node)
   print(string.format("%d: %s", node.type, node.s))
   -- print(node.t .. ": (" .. tostring(node.line) .. ":" .. tostring(node.col) .. ")")
end

return {
   walk = walk,
   emit = emit,
   test = test
}
