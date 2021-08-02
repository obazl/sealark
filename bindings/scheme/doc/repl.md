# sunlark repl

Not much here yet, but the examples below should suffice to get you started.

**WARNING**: only tested on MacOS.  Will test on Linux in the next few days.

## repl examples

* [projection](#projection)
* [mutation](#mutation)
* [serialization](#serialization) (and print representations)

### <a name="projection">projection</a> (getters)

Using [test/unit/sunlark/BUILD.mock](../../../test/unit/sunlark/BUILD.mock) to demonstrate a subset of supported projection operators:

```
[:~/sealark]$ bazel run sunlark:repl
...
s7> (define ast (sunlark-parse-build-file "test/unit/sunlark/BUILD.mock"))
ast
s7> (define target-0 (ast :target 0))  ## first target in file
target-0
s7> target-0
(def-target :name "hello-lib" :rule cc_library
  :attrs ((name . "hello-lib")
          (srcs . #("hello-lib.cc" "howdy.cc" "howdy.h"))
          (hdrs . #("hello-lib.h"))
          (defines . 'DEFINES)))
s7> (define h-lib (ast :target "hello-lib")) ## same, by name
h-lib
s7> h-lib
(def-target :name "hello-lib" :rule cc_library
  :attrs ((name . "hello-lib")
          (srcs . #("hello-lib.cc" "howdy.cc" "howdy.h"))
          (hdrs . #("hello-lib.h"))
          (defines . 'DEFINES)))
s7> (equal? target-0 h-lib)
#t
s7> (eq? target-0 h-lib)
#f
s7> (define t0 (ast :> 0)) ## abbrev   :> == :target
t0
s7> (equal? t0 target-0)
#t
s7> (define tlast (ast :> -1))  ## last target in file
tlast
s7> tlast
(def-target :name "ints" :rule sunlark_mock
  :attrs ((name . "ints")
          (false_attr .  #f)
          (int_attr . 99)
          (int_list . #(1 2 3))))
s7> (tlast :@@)           ##  == (tlast :bindings) == (tlast :attrs)
 :attrs ((name . "ints")
        (false_attr .  #f)
        (int_attr . 99)
        (int_list . #(1 2 3)))
s7> (tlast :@ 0)  ## first attrib
(name . "ints")
s7> (tlast :@ -1) ## last attrib
(int_list . #(1 2 3))
s7> (tlast :@ 'int_list)  ## index by name
(int_list . #(1 2 3))
s7> (tlast :@ 'int_list :value)  ## just the value part
#(1 2 3)
s7> (tlast :@ 'int_list :value 1) ## index the value list
2
s7> (ast :> -1 :@ 'int_list :value 1)  ## w/out intermediate vars
2
## using another intermediate var:
s7> (define binding (tlast :@ 'int_list))
binding
s7> binding
(int_list . #(1 2 3))
s7> (binding :key)
int_list
s7> (binding :value)
#(1 2 3)
s7> (binding :value -1)
3
## alternatively:
s7> (define bs (tlast :@@)) ## == (tlast :bindings) == (tlast :attrs)
bs
s7> bs
 :attrs ((name . "ints")
        (false_attr .  #f)
        (int_attr . 99)
        (int_list . #(1 2 3)))
s7> (define b (bs -1))  ## last binding in list
b
s7> b
(int_list . #(1 2 3))
s7>
```

Similarly for load statements (:loads, :load), although the output formatting needs some work:

```
s7> (define ast (sunlark-parse-build-file "test/unit/sunlark/BUILD.mock"))
ast
s7> (ast :loads)
( (def-load :src "@rules_cc//cc:defs.bzl"
        :args ("cc_binary" "cc_library" "cc_test")
)  (def-load :src "@arepo//apkg:afile.bzl"
        :args ("arg1c" "arg2c" "arg3c")
        :bindings ((key1c . "val1c") (key2c . "val2c") (lastkey3c . "lastval3c"))))
s7>
```

Projection operators (:src, :args, :bindings) for load statement
objects are undergoing testing and should be done soon.

Properties for the remaining top-level directives are under development (:definitions, :def-var, :def-vars, :funcall, :procedure, etc.)

### <a name="mutation">mutation</a> (setters)

Mutation combines Scheme\'s standard `set!` operator with Sunlark projection ops; schematically:

    (set! (obj path) newval)

Replace an attribute value:

```
### first inspect:
s7> (define ast (sunlark-parse-build-file "test/unit/sunlark/BUILD.mock"))
ast
s7> (ast :> 0 :@ 1)
(srcs . #("hello-lib.cc" "howdy.cc" "howdy.h"))
s7> (ast :> 0 :@ 1 :$)
#("hello-lib.cc" "howdy.cc" "howdy.h")
s7> (ast :> 0 :@ 1 :$ 1)
"howdy.cc"
### now mutate:
s7> (ast :> 0)   ## before
(def-target :name "hello-lib" :rule cc_library
  :attrs ((name . "hello-lib")
          (srcs . #("hello-lib.cc" "howdy.cc" "howdy.h"))
          (hdrs . #("hello-lib.h"))
          (defines . 'DEFINES)))
s7> (set! (ast :> 0 :@ 1 :$) "goodbye") # set value of attr 1 of target 0
(srcs . "goodbye")
s7> (ast :> 0)  ## after
(def-target :name "hello-lib" :rule cc_library
  :attrs ((name . "hello-lib")
          (srcs . "goodbye")          ## <<< replaced
          (hdrs . #("hello-lib.h"))
          (defines . 'DEFINES)))
## using names, replace val of srcs attrib with a list:
s7> (set! (ast :> "hello-lib" :@ 'srcs :$) '("hello" "again")))
#("hello" "again")
s7> (ast :> 0)
(def-target :name "hello-lib" :rule cc_library
  :attrs ((name . "hello-lib")
          (srcs . #("hello" "again"))
          (hdrs . #("hello-lib.h"))
          (defines . 'DEFINES)))
```

Replace an item in a list value:

```
s7> (define ast (sunlark-parse-build-file "test/unit/sunlark/BUILD.mock"))
ast
s7> (define tlast (ast :> -1))
tlast
s7> tlast
(def-target :name "ints" :rule sunlark_mock
  :attrs ((name . "ints")
          (false_attr .  #f)
          (int_attr . 99)
          (int_list . #(1 2 3))))
s7> (set! (tlast :binding 'int_list :value 1) 5)
#(1 5 3)
s7> tlast
(def-target :name "ints" :rule sunlark_mock
  :attrs ((name . "ints")
          (false_attr .  #f)
          (int_attr . 99)
          (int_list . #(1 5 3))))
## same thing, with abbrevs: :@ == binding, :$ == :value
s7> (set! (tlast :@ -1 :$ 2) 71)
#(1 5 71)
s7> tlast
(def-target :name "ints" :rule sunlark_mock
  :attrs ((name . "ints")
          (false_attr .  #f)
          (int_attr . 99)
          (int_list . #(1 5 71))))
```

### <a name="serialization">serialization</a> (getters)

The default display output shown above is just a lispy representation
of the AST, which is stored as a C structure. You can also serialize to starlark, or, for debugging purposes, a concise AST outline.

To serialize to Starlark use `sunlark->starlark`:

```
s7> (define ast (sunlark-parse-build-file "test/unit/sunlark/BUILD.mock"))
ast
s7> (sunlark->starlark ast)
"package(default_visibility = [\"//visibility:public\"])

load(\"@rules_cc//cc:defs.bzl\", \"cc_binary\", \"cc_library\", \"cc_test\")

load(\"@arepo//apkg:afile.bzl\", \"arg1c\", \"arg2c\", \"arg3c\",
    key1c = \"val1c\", key2c = \"val2c\", lastkey3c = \"lastval3c\",
)

DEFINES = [\"DEBUG\", \"FOO\"]

a, b = 25, 30  ## multiple assignment parse test

cc_library(
    name = \"hello-lib\",
    srcs = [\"hello\",\"again\"]                      ,
    hdrs = [\"hello-lib.h\"],
    defines = DEFINES
)

cc_binary(
    name = \"hello-world\",
    srcs = [\"hello-world.cc\"],
    deps = [\":hello-lib\"],
    defines = DEFINES
)

cc_test(
    name = \"hello-success_test\",
    srcs = [\"hello-world.cc\"],
    deps = [\":hello-lib\"],
)

cc_test(
    name = \"hello-fail_test\",
    srcs = [\"hello-fail.cc\"],
    deps = [\":hello-lib\"],
)

sunlark_mock(
    name = \"strings\",
    true_attr = True,
    str_attr = \"hello\",
    strlist_attr = [\"a\", \"b\", \"c\"],
    strlist_long = [\"a\",
                    long, list, including,
                    \"variables\", \"and\", \"strings\", \"for\",
                    testing],
)

sunlark_mock(
    name = \"ints\",
    false_attr = False,
    int_attr = 99,
    int_list = [1, 2, 3]
)
"
s7>
```

To print an AST outline use the `:ast` operator. Each output line shows the node type and code number (in [ ]), the @line:col position, and the string value if there is one.

```
s7> (ast :ast)
"0: TK_Build_File[96] @0:0
  1: TK_Stmt_List[131] @0:0
    2: TK_Small_Stmt_List[129] @0:0
      3: TK_Expr_List[109] @0:0
        4: TK_Call_Expr[97] @0:0
          5: TK_ID[37] @0:0    package
          5: TK_Call_Sfx[98] @0:7
            6: TK_LPAREN[54] @0:7
            6: TK_Arg_List[87] @0:8
              7: TK_Binding[88] @0:8
                8: TK_ID[37] @0:8    default_visibility
                8: TK_EQ[26] @0:27
                8: TK_List_Expr[116] @0:29
                  9: TK_LBRACK[49] @0:29
                  9: TK_Expr_List[109] @0:30
                    10: TK_STRING[79] @0:30    \"//visibility:public\"
                  9: TK_RBRACK[69] @0:51
            6: TK_RPAREN[71] @0:52
      3: TK_Load_Stmt[117] @2:0
        4: TK_LOAD[53] @2:0
        4: TK_LPAREN[54] @2:4
        4: TK_STRING[79] @2:5    \"@rules_cc//cc:defs.bzl\"
        4: TK_COMMA[15] @2:29
        4: TK_STRING[79] @2:31    \"cc_binary\"
        4: TK_COMMA[15] @2:42
        4: TK_STRING[79] @2:44    \"cc_library\"
        4: TK_COMMA[15] @2:56
        4: TK_STRING[79] @2:58    \"cc_test\"
        4: TK_RPAREN[71] @2:67
      3: TK_Load_Stmt[117] @4:0
        4: TK_LOAD[53] @4:0
        4: TK_LPAREN[54] @4:4
        4: TK_STRING[79] @4:5    \"@arepo//apkg:afile.bzl\"
        4: TK_COMMA[15] @4:29
        4: TK_STRING[79] @4:31    \"arg1c\"
        4: TK_COMMA[15] @4:38
        4: TK_STRING[79] @4:40    \"arg2c\"
        4: TK_COMMA[15] @4:47
        4: TK_STRING[79] @4:49    \"arg3c\"
        4: TK_COMMA[15] @4:56
        4: TK_Binding[88] @5:4
          5: TK_ID[37] @5:4    key1c
          5: TK_EQ[26] @5:10
          5: TK_STRING[79] @5:12    \"val1c\"
        4: TK_COMMA[15] @5:19
        4: TK_Binding[88] @5:21
          5: TK_ID[37] @5:21    key2c
          5: TK_EQ[26] @5:27
          5: TK_STRING[79] @5:29    \"val2c\"
        4: TK_COMMA[15] @5:36
        4: TK_Binding[88] @5:38
          5: TK_ID[37] @5:38    lastkey3c
          5: TK_EQ[26] @5:48
          5: TK_STRING[79] @5:50    \"lastval3c\"
        4: TK_COMMA[15] @5:61
        4: TK_RPAREN[71] @6:0
      3: TK_Assign_Stmt[94] @8:0
        4: TK_Expr_List[109] @8:0
          5: TK_ID[37] @8:0    DEFINES
        4: TK_EQ[26] @8:8
        4: TK_Expr_List[109] @8:10
          5: TK_List_Expr[116] @8:10
            6: TK_LBRACK[49] @8:10
            6: TK_Expr_List[109] @8:11
              7: TK_STRING[79] @8:11    \"DEBUG\"
              7: TK_COMMA[15] @8:18
              7: TK_STRING[79] @8:20    \"FOO\"
            6: TK_RBRACK[69] @8:25
      3: TK_Assign_Stmt[94] @10:0
        4: TK_Expr_List[109] @10:0
          5: TK_ID[37] @10:0    a
          5: TK_COMMA[15] @10:1
          5: TK_ID[37] @10:3    b
        4: TK_EQ[26] @10:5
        4: TK_Expr_List[109] @10:7
          5: TK_INT[40] @10:7        25
          5: TK_COMMA[15] @10:9
          5: TK_INT[40] @10:11        30
      3: TK_Expr_List[109] @12:0
        4: TK_Call_Expr[97] @12:0
          5: TK_ID[37] @12:0    cc_library
          5: TK_Call_Sfx[98] @12:10
            6: TK_LPAREN[54] @12:10
            6: TK_Arg_List[87] @13:4
              7: TK_Binding[88] @13:4
                8: TK_ID[37] @13:4    name
                8: TK_EQ[26] @13:9
                8: TK_STRING[79] @13:11    \"hello-lib\"
              7: TK_COMMA[15] @13:22
              7: TK_Binding[88] @14:4
                8: TK_ID[37] @14:4    srcs
                8: TK_EQ[26] @14:9
                8: TK_List_Expr[116] @14:11
                  9: TK_LBRACK[49] @0:0
                  9: TK_Expr_List[109] @0:0
                    10: TK_STRING[79] @0:0    \"hello\"
                    10: TK_COMMA[15] @0:0
                    10: TK_STRING[79] @0:0    \"again\"
                  9: TK_RBRACK[69] @0:0
              7: TK_COMMA[15] @14:50
              7: TK_Binding[88] @15:4
                8: TK_ID[37] @15:4    hdrs
                8: TK_EQ[26] @15:9
                8: TK_List_Expr[116] @15:11
                  9: TK_LBRACK[49] @15:11
                  9: TK_Expr_List[109] @15:12
                    10: TK_STRING[79] @15:12    \"hello-lib.h\"
                  9: TK_RBRACK[69] @15:25
              7: TK_COMMA[15] @15:26
              7: TK_Binding[88] @16:4
                8: TK_ID[37] @16:4    defines
                8: TK_EQ[26] @16:12
                8: TK_ID[37] @16:14    DEFINES
            6: TK_RPAREN[71] @17:0
      3: TK_Expr_List[109] @19:0
        4: TK_Call_Expr[97] @19:0
          5: TK_ID[37] @19:0    cc_binary
          5: TK_Call_Sfx[98] @19:9
            6: TK_LPAREN[54] @19:9
            6: TK_Arg_List[87] @20:4
              7: TK_Binding[88] @20:4
                8: TK_ID[37] @20:4    name
                8: TK_EQ[26] @20:9
                8: TK_STRING[79] @20:11    \"hello-world\"
              7: TK_COMMA[15] @20:24
              7: TK_Binding[88] @21:4
                8: TK_ID[37] @21:4    srcs
                8: TK_EQ[26] @21:9
                8: TK_List_Expr[116] @21:11
                  9: TK_LBRACK[49] @21:11
                  9: TK_Expr_List[109] @21:12
                    10: TK_STRING[79] @21:12    \"hello-world.cc\"
                  9: TK_RBRACK[69] @21:28
              7: TK_COMMA[15] @21:29
              7: TK_Binding[88] @22:4
                8: TK_ID[37] @22:4    deps
                8: TK_EQ[26] @22:9
                8: TK_List_Expr[116] @22:11
                  9: TK_LBRACK[49] @22:11
                  9: TK_Expr_List[109] @22:12
                    10: TK_STRING[79] @22:12    \":hello-lib\"
                  9: TK_RBRACK[69] @22:24
              7: TK_COMMA[15] @22:25
              7: TK_Binding[88] @23:4
                8: TK_ID[37] @23:4    defines
                8: TK_EQ[26] @23:12
                8: TK_ID[37] @23:14    DEFINES
            6: TK_RPAREN[71] @24:0
      3: TK_Expr_List[109] @26:0
        4: TK_Call_Expr[97] @26:0
          5: TK_ID[37] @26:0    cc_test
          5: TK_Call_Sfx[98] @26:7
            6: TK_LPAREN[54] @26:7
            6: TK_Arg_List[87] @27:4
              7: TK_Binding[88] @27:4
                8: TK_ID[37] @27:4    name
                8: TK_EQ[26] @27:9
                8: TK_STRING[79] @27:11    \"hello-success_test\"
              7: TK_COMMA[15] @27:31
              7: TK_Binding[88] @28:4
                8: TK_ID[37] @28:4    srcs
                8: TK_EQ[26] @28:9
                8: TK_List_Expr[116] @28:11
                  9: TK_LBRACK[49] @28:11
                  9: TK_Expr_List[109] @28:12
                    10: TK_STRING[79] @28:12    \"hello-world.cc\"
                  9: TK_RBRACK[69] @28:28
              7: TK_COMMA[15] @28:29
              7: TK_Binding[88] @29:4
                8: TK_ID[37] @29:4    deps
                8: TK_EQ[26] @29:9
                8: TK_List_Expr[116] @29:11
                  9: TK_LBRACK[49] @29:11
                  9: TK_Expr_List[109] @29:12
                    10: TK_STRING[79] @29:12    \":hello-lib\"
                  9: TK_RBRACK[69] @29:24
            6: TK_COMMA[15] @29:25
            6: TK_RPAREN[71] @30:0
      3: TK_Expr_List[109] @32:0
        4: TK_Call_Expr[97] @32:0
          5: TK_ID[37] @32:0    cc_test
          5: TK_Call_Sfx[98] @32:7
            6: TK_LPAREN[54] @32:7
            6: TK_Arg_List[87] @33:4
              7: TK_Binding[88] @33:4
                8: TK_ID[37] @33:4    name
                8: TK_EQ[26] @33:9
                8: TK_STRING[79] @33:11    \"hello-fail_test\"
              7: TK_COMMA[15] @33:28
              7: TK_Binding[88] @34:4
                8: TK_ID[37] @34:4    srcs
                8: TK_EQ[26] @34:9
                8: TK_List_Expr[116] @34:11
                  9: TK_LBRACK[49] @34:11
                  9: TK_Expr_List[109] @34:12
                    10: TK_STRING[79] @34:12    \"hello-fail.cc\"
                  9: TK_RBRACK[69] @34:27
              7: TK_COMMA[15] @34:28
              7: TK_Binding[88] @35:4
                8: TK_ID[37] @35:4    deps
                8: TK_EQ[26] @35:9
                8: TK_List_Expr[116] @35:11
                  9: TK_LBRACK[49] @35:11
                  9: TK_Expr_List[109] @35:12
                    10: TK_STRING[79] @35:12    \":hello-lib\"
                  9: TK_RBRACK[69] @35:24
            6: TK_COMMA[15] @35:25
            6: TK_RPAREN[71] @36:0
      3: TK_Expr_List[109] @38:0
        4: TK_Call_Expr[97] @38:0
          5: TK_ID[37] @38:0    sunlark_mock
          5: TK_Call_Sfx[98] @38:12
            6: TK_LPAREN[54] @38:12
            6: TK_Arg_List[87] @39:4
              7: TK_Binding[88] @39:4
                8: TK_ID[37] @39:4    name
                8: TK_EQ[26] @39:9
                8: TK_STRING[79] @39:11    \"strings\"
              7: TK_COMMA[15] @39:20
              7: TK_Binding[88] @40:4
                8: TK_ID[37] @40:4    true_attr
                8: TK_EQ[26] @40:14
                8: TK_ID[37] @40:16    True
              7: TK_COMMA[15] @40:20
              7: TK_Binding[88] @41:4
                8: TK_ID[37] @41:4    str_attr
                8: TK_EQ[26] @41:13
                8: TK_STRING[79] @41:15    \"hello\"
              7: TK_COMMA[15] @41:22
              7: TK_Binding[88] @42:4
                8: TK_ID[37] @42:4    strlist_attr
                8: TK_EQ[26] @42:17
                8: TK_List_Expr[116] @42:19
                  9: TK_LBRACK[49] @42:19
                  9: TK_Expr_List[109] @42:20
                    10: TK_STRING[79] @42:20    \"a\"
                    10: TK_COMMA[15] @42:23
                    10: TK_STRING[79] @42:25    \"b\"
                    10: TK_COMMA[15] @42:28
                    10: TK_STRING[79] @42:30    \"c\"
                  9: TK_RBRACK[69] @42:33
              7: TK_COMMA[15] @42:34
              7: TK_Binding[88] @43:4
                8: TK_ID[37] @43:4    strlist_long
                8: TK_EQ[26] @43:17
                8: TK_List_Expr[116] @43:19
                  9: TK_LBRACK[49] @43:19
                  9: TK_Expr_List[109] @43:20
                    10: TK_STRING[79] @43:20    \"a\"
                    10: TK_COMMA[15] @43:23
                    10: TK_ID[37] @44:20    long
                    10: TK_COMMA[15] @44:24
                    10: TK_ID[37] @44:26    list
                    10: TK_COMMA[15] @44:30
                    10: TK_ID[37] @44:32    including
                    10: TK_COMMA[15] @44:41
                    10: TK_STRING[79] @45:20    \"variables\"
                    10: TK_COMMA[15] @45:31
                    10: TK_STRING[79] @45:33    \"and\"
                    10: TK_COMMA[15] @45:38
                    10: TK_STRING[79] @45:40    \"strings\"
                    10: TK_COMMA[15] @45:49
                    10: TK_STRING[79] @45:51    \"for\"
                    10: TK_COMMA[15] @45:56
                    10: TK_ID[37] @46:20    testing
                  9: TK_RBRACK[69] @46:27
            6: TK_COMMA[15] @46:28
            6: TK_RPAREN[71] @47:0
      3: TK_Expr_List[109] @49:0
        4: TK_Call_Expr[97] @49:0
          5: TK_ID[37] @49:0    sunlark_mock
          5: TK_Call_Sfx[98] @49:12
            6: TK_LPAREN[54] @49:12
            6: TK_Arg_List[87] @50:4
              7: TK_Binding[88] @50:4
                8: TK_ID[37] @50:4    name
                8: TK_EQ[26] @50:9
                8: TK_STRING[79] @50:11    \"ints\"
              7: TK_COMMA[15] @50:17
              7: TK_Binding[88] @51:4
                8: TK_ID[37] @51:4    false_attr
                8: TK_EQ[26] @51:15
                8: TK_ID[37] @51:17    False
              7: TK_COMMA[15] @51:22
              7: TK_Binding[88] @52:4
                8: TK_ID[37] @52:4    int_attr
                8: TK_EQ[26] @52:13
                8: TK_INT[40] @52:15        99
              7: TK_COMMA[15] @52:17
              7: TK_Binding[88] @53:4
                8: TK_ID[37] @53:4    int_list
                8: TK_EQ[26] @53:13
                8: TK_List_Expr[116] @53:15
                  9: TK_LBRACK[49] @53:15
                  9: TK_Expr_List[109] @53:16
                    10: TK_INT[40] @53:16        1
                    10: TK_COMMA[15] @53:17
                    10: TK_INT[40] @53:19        2
                    10: TK_COMMA[15] @53:20
                    10: TK_INT[40] @53:22        3
                  9: TK_RBRACK[69] @53:23
            6: TK_RPAREN[71] @54:0
"
```
