## test obazl lua plugin (lib)
dune = require 'obazl.dune'

pkg = dune.parse_file('data/dune/libs.dune')

print("stanzas")
for k,stanza in pairs(pkg.stanzas) do
   print(k,stanza)
   print("stanza:")
   for k,v in pairs(stanza) do
      print("entry:", k,v)
      if type(v) == "table" then
         print("pairs(val):")
         for j,fld in pairs(v) do
            print(j, fld)
            -- for name,val in pairs(fld) do
            --    print(string.format("\t%s = %s", name, val))
            -- end
         end
      end
   end
end

-- stanzas = pkg.stanzas

-- s1 = stanzas[1]
-- s2 = stanzas[2]

-- flds = s1.fields

-- libs = s1.libraries

-- for k,v,s in pairs(s1) do print(k, v, s) end

-- t = { "a", "b" }

lustache = require "lustache"

-- t_stanzas = [[
-- {{{path}}}
-- {{#stanzas}}
-- stanza: {{type}}

-- {{#fields}}
-- {{#name}}name = {{name}}
-- {{/name}}{{!
-- }}{{#public_name}}public_name = {{public_name}}
-- {{/public_name}}{{!
-- }}{{#modules}}modules = {{modules}}
-- {{/modules}}{{!
-- }}{{#preprocess}}preprocess = {{preprocess}}
-- {{/preprocess}}{{!
-- }}{{/fields}}

-- {{/stanzas}}
-- ]]

txx = [[
{{{path}}}
{{#stanzas}}

==== stanza: {{type}} ====
{{#fields}}
{{#name}}name = {{name}}
{{/name}}{{!
}}{{#public_name}}public name = {{public_name}}
{{/public_name}}{{!
}}{{#libraries}}libraries = [
    {{#list}} "{{item}}"{{^last}}, {{/last}}{{/list}}
],
{{/libraries}}{{!
}}{{#modules}}modules = [
{{#include}}    include: {{#list}} "{{item}}"{{^last}}, {{/last}}{{/list}}
{{/include}}{{!
}}{{#exclude}}    exclude: {{#list}} "{{item}}"{{^last}}, {{/last}}{{/list}}
{{/exclude}}{{!
}}{{#resolved}}    resolved: {{#list}} "{{item}}"{{^last}}, {{/last}}{{/list}}
{{/resolved}}
],
{{/modules}}{{!
}}{{/fields}}
{{/stanzas}}
]]

testdata = {
   stanzas = {
      { type = 3,
        fields = {
           -- { type = 0, --name
             name = "foo",
           -- },
           -- { type = 1, --name
             name = "publicfoo",
           -- },
           -- { -- type = 2, -- libraries
             libraries = {
                list = {
                   {item="liba"}, {item="libb"}, {item="libc", last=true}
                }
             },
           -- },
           -- {
              modules = {
                 include = {
                    list = { {item="imoda"}, {item="imodb"}, {item="imodc", last=true} }
                 },
                 exclude = {
                    list = { {item="xmoda"}, {item="xmodb"}, {item="xmodc", last=true} }
                 },
                 resolved = {
                    list = { {item="rmoda"}, {item="rmodb"}, {item="rmodc", last=true} }
                 }
              }
           -- }
        }
      }
   }
}

print("================")
ms = pkg.stanzas[1].fields
print("ms: ", ms.modules.exclude.list[1].item)
ms.modules.resolved = {list = { {item="ra"}, {item="rb"}, {item="rc"}}}
print("ms: ", ms.modules.resolved.list)
print("================")
print()
-- .resolved.list, {item="resA"})

print("\nlustache test on pkg\n")
print(lustache:render(txx, pkg))

-- print("\nlustache test on testdata\n")
-- print(lustache:render(txx, testdata))

-- print()
-- for k,v in pairs(pkg.stanzas[1].fields) do
--    print(k,v)
--    for kk,vv in pairs(v) do
--       print(kk,vv)
--       if kk == "libraries" then
--          for kkk,vvv in ipairs(vv.list) do
--             print(kkk,vvv)
--          end
--          -- print("list", vv.list)
--       end
--    end
-- end


-- t_stanza = [[
-- type: {{type}}
-- {{#fields}}
-- fld
-- {{#name}} name: {{name}}{{/name}}
-- {{#public_name}} public name: {{name}}{{/name}}

-- {{/fields}}
-- ]]
-- print("\nlustache test on s1\n")
-- print(lustache:render(t_stanza, s1))


-- flds = s2.fields

-- -- for k,v in pairs(flds) do
-- --    print(k,v)
-- --    for kk,vv in pairs(v) do
-- --       print(kk,vv)
-- --       print(v.name)
-- --    end
-- -- end

-- tx = [[
-- Shown.
-- {{#people}}
-- {{#person}}
-- n: {{person}}
-- {{/person}}
-- {{/people}}
-- ]]

-- dx = {
--    people = {
--       { person = false},
--       { person = "alice" }
--    }
-- }

-- print(lustache:render(tx,dx))
