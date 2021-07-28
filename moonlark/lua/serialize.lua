-- moonlark_serialize.lua

-- print("hello from moonlark/lua/serialize.lua")

ast_walk = require "ast_walk"

line = 0
col  = 0

function emit_node_starlark(node)
   -- if printable
   if (moonlark.pTOK[node.type]) then
      i = line
      while (i < node.line) do
         outfile:write("\n")
         line = line + 1
         col = 0
         i = i + 1
      end
      while (col < node.col) do
         outfile:write(" ")
         col = col + 1
      end
      if (node.type == moonlark.TOK.STRING) then
         extra = 0
         if (node.binary) then outfile:write("b"); extra = extra + 1 end
         if (node.raw) then outfile:write("r"); extra = extra + 1 end
         if (node.qq == 3) then
            outfile:write(node.q); outfile:write(node.q); outfile:write(node.q)
         else
            outfile:write(node.q)
         end
         outfile:write(node.s)
         if (node.qq == 3) then
            outfile:write(node.q)
            outfile:write(node.q)
            outfile:write(node.q)
         else
            outfile:write(node.q)
         end
         if (node.qq == 3) then
            extra = extra + 6
         else
            extra = extra + 2
         end
         col = col + #(node.s) + extra
      elseif (node.type == moonlark.TOK.ID) then
         outfile:write(node.s)
         col = col + #(node.s)
      elseif (node.type == moonlark.TOK.INT) then
         outfile:write(node.s)
         col = col + #(node.s)
      elseif (node.type == moonlark.TOK.FLOAT) then
         outfile:write(node.s)
         col = col + #(node.s)
      elseif (node.type == moonlark.TOK.COMMENT) then
         outfile:write(node.s)
         col = col + #(node.s)
      else
         s = moonlark.pTOK[node.type]
         outfile:write(s)
         col = col + #s
      end
   end
   return true
end

function emit_starlark(node, ofile)
   handlers = {}
   handlers.default = emit_node_starlark
   outfile = assert(io.open(ofile, "w+"))
   -- outfile:write("SERIALIZED\n")
   ast_walk.all(node, handlers)
   outfile:write("\n")
   io.close(outfile)
end

return {
   emit_starlark = emit_starlark,
   emit_node_starlark = emit_node_starlark
}
