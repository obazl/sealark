-- moonlark_serialize.lua

print("hello from moonlark_serialize.lua")

function node_handler(node)
   if (bazel.pTOK[node.type]) then
      print("node type " .. bazel.iTOK[node.type] .. ": " .. bazel.pTOK[node.type])
   elseif (node.type == bazel.TOK.STRING) then
      print("STRING: " .. node.s)
   elseif (node.type == bazel.TOK.ID) then
      print("ID: " .. node.s)
   end
end

-- FIXME: put 'walk' in a different lib ('ast.lua'?)
function walk(t, handlers)
   if (handlers[t.type]) then
      handlers[t.type](t)
   else
      handlers.default(t)
   end
     -- for k,v in pairs(t) do
     --    print(k,v)
     -- end
     if (t.subnodes) then
        for k,v in ipairs(t.subnodes) do
           -- print("subnode " .. k)
           walk(v, handlers)
        end
     end
end

function test(node)
   for k,v in pairs(bazel.TOK) do
      print(k,v)
   end

   handlers = {}
   handlers.default = node_handler
   walk(node, handlers)
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
