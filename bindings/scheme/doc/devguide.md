# sunlark developer's guide

## BUILD file path expression DSL

Top level productions:

* :directives - in practice, all "call expressions" at the top level
  serve as directives

  * :targets - target directives; call expressions with "name" binding,
  no singleton args

  * :loads - load directives, call expressions named 'load'

  * :package - the single "package" directive

  * :procedures - all call expressions excluding :targets, :loads, :package

* :definitions - variable defs. In bzl files, definitions include
  :function-definitions and :variable-definitions

* other?

## Build file nodes (tid :build_file)

### one arg

must be one of the top-level directives:

* :directives
* :targets
* :loads
* :package
* :prodecures
* :definitions

returns Scheme list

### dyads

first arg: as above

Interpretation of the second arg depends on first arg.

* integer - indexes result of first arg, e.g. `:target 1` => second target

* string - indexes result of first arg using string as key.
  Interpretation of key depends on first arg, e.g. `:target "foo"` =>
  target with name = "foo", but `:load "@arepo//apkg:afile.bzl"` =>
  load statement whose first arg is "@arepo//apkg:afile.bzl".

* symbol - following :target only: filters the list by rule, e.g.
  `:targets 'cc_library` selects the targets whose rule is
  `cc_library`.

* list - list of predicates filters the result of the first arg.
  Elements will be selected if they satisfy one of the predicates.
    Supported predicates:

  * integer selects by position in list
  * string selects by key
  * symbol (after :targets) selects by rule


* predicate function - filters list, selecting elements satisfying predicate

ADVICE: use singular when there is an int or string second arg,
`:target 1`, ("the second target"), `:target "foo"` ("the foo
target"), but plural otherwise, e.g `:targets` ("the targets"),
`:targets 'cc_library` ("the cc_library targets").



Examples:

      :targets <integer>

          sunlark_target_for_index => target node

      :target "string"
          returns target node for target with name = "string"
          sunlark_target_for_name => target node

      :targets 'rule
          filters targets by rulename, e.g. (ast :targets 'cc_binary)
          sunlark_targets_for_symbol => scheme list of target nodes

      :targets <list of syms strings integers>
          list selects by rule, name, and/or index
          sunlark_targets_for_filter => scheme list of target nodes

      :targets <predicate>
          predicate function selects targets
          sunlark_targets_for_lambda => scheme list of target nodes

      :load "string"
          returns node for load stmt whose first arg is string
          => sunlark_load_for_name


### triads

first to args: as above

third arg:


   :targets 'sym <int>
          sunlark_target_for_index_from_filtersym

   :targets <list> <int>
          sunlark_target_for_index_from_filterlist

   :targets <int> :bindings
       returns just the bindings w/o comma nodes
       sunlark_bindings_for_target_for_index

   :targets <int> :arg-list
       returns entire ast list, including punctuation nodes
       sunlark_arglist_for_target_for_index

   :targets <int> :rule
       returns rule id for target at index
       sunlark_rule_for_target_for_index

   :target "string" :bindings
          returns bindings without commas
          sunlark_bindings_for_target_for_name => scheme list of bindings

   :target "string" :arg-list
          returns entire arg list including punctuation
          sunlark_bindings_for_target_for_name => scheme list of args

   :target "name" :rule
       returns rule id for target named
       sunlark_ruleid_for_target_for_name => ruleid node

   :targets <selector> :count  (not implemented, use length function)
          <selector> :: filter list, rule sym
          sunlark_count_targets_from_filter

### tetrads

first three args: as above

fourth arg:

  :target <selector> :bindings 'key
          returns node of binding with key "key"
          sunlark_binding_for_key => :binding node

  :target <selector> :bindings <int>
          returns ith binding
          sunlark_binding_for_index => :binding node

### pentads

first four: as above

fifth arg:

  :target <selector> :bindings 'mykey :key
      returns key node for binding with key 'mykey' (for updating)
      sunlark_key_for_key...

  :target <selector> :bindings 'mykey :value
      returns value node for binding with key 'mykey' (for updating)
         sunlark_value_for_binding_from_bindings_from_filter

## misc

/*

  edit dsl:

  (set! (node ref) (:<op> :<location> <edits>)

  :<op> == one of :add, :remove, :replace

  :<location> ==
      :n for index, e.g. :0 for first, :1 second, :-0 last
      "foo" for value match, e.g. (:replace "foo" "bar")
      :* for everything, e.g. (:remove :*) results in []
      <location> is optional for :add; default is :-0 (append)

  <edits> == a string or number

  examples:

      '(:add :0 "foo") ;; insert "foo" in first position
      '(:add "foo") ;; appends, same as (:add :-0 "foo")
      '(:replace :2 "foo") ;; replaces 3rd element with "foo"
      '(:replace "foo" "bar") ;; "bar" replaces "foo"
      '(:replace :* "foo") ;; result is ["foo"]
      '(:remove :1) ;; removes second element
      '(:remove "foo") ;; removes "foo"
      '(:remove :*) ;; result is []

      we need ' to prevent eval of keywords in fn position.
      alternative: reader macros, e.g. #+, #-, #!
 */

/*
          ;; to update a string-list attrib value: pass a list?
          ;; we need both an update! and a replace! function
          ;; in the set! context, (deps 'value) returns
          ;; the updatable value, and we can do anything
          ;; with it; we're not required to replace it in toto.
          ;; (set! (deps 'value) 99) ;; fail, wrong type
          ;; or

          (set! (deps 'value) '(:append "dep_x")) ;; ok, adds to 99 to list
          ;; here :append is metadata, a command. but what if
          ;; we want to add :append to the value?
          ;; use a hashtable instead of a list?
          ;; or a custom read macro?

          (set! (deps 'value) #+("dep_x")) ;; adds arg to val list
          (set! (deps 'value) #-("dep_x")) ;; removes arg from val list
          (set! (deps 'value) #!("dep_x")) ;; replaces val list

          ;; #+("depx") expands to (:::private_append_sym "depx")
          ;; or:  (#+(append) "depx"), (#+(remove) "depx") etc.
          ;; and #+(append) => '___append, or some such
          ;; maybe #@(...), since this is for attibutes

          ;; @+() works for strings too

          ;; what about regex replace expressions?
          ;; e.g. for each item in list, s/foo/bar/

          ;; (set! (deps 'value) "dep_x" :insert-after "dep_a") ;; ok, adds to 99 to list
          ;; (set! (deps 'value) 99 :replace) ;; replace entire value
          ;; ;;
          ;; (ast-node-replace! deps 'value 99) ;; ok
          ;; (set! (deps 'value) 99) ;; #(foo bar bazl))
*/
