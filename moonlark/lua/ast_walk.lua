-- ast_walk.lua

print("hello from moonlark/lua/ast_walk.lua")

function walk_all(t, handlers)
   if (handlers[t.type]) then
      handlers[t.type](t)
   else
      handlers.default(t)
   end
   if (t.comments) then
      for k,v in ipairs(t.comments) do
         walk_all(v, handlers)
      end
   end
   if (t.subnodes) then
      for k,v in ipairs(t.subnodes) do
         walk_all(v, handlers)
      end
   end
end

function walk_rules(t, handlers)
   if (handlers[t.type]) then
      handlers[t.type](t)
   else
      handlers.default(t)
   end
   if (t.comments) then
      for k,v in ipairs(t.comments) do
         walk(v, handlers)
      end
   end
   if (t.subnodes) then
      for k,v in ipairs(t.subnodes) do
         walk(v, handlers)
      end
   end
end

function walk_toplevel(t, handlers)
   if (handlers[t.type]) then
      handlers[t.type](t)
   else
      handlers.default(t)
   end
   if (t.comments) then
      for k,v in ipairs(t.comments) do
         walk(v, handlers)
      end
   end
   if (t.subnodes) then
      for k,v in ipairs(t.subnodes) do
         walk(v, handlers)
      end
   end
end

return {
   all   = walk_all,
   rules = walk_rules,
   toplevel = walk_toplevel,
}
