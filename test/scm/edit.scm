(display "hello from .sunlark.d/edit.scm")
(newline)

(load "ast_walk.scm")

(define handlers ()
  )

(define (test-subnodes ast)
  (let ((subnodes (ast :subnodes)))
    (display "list subnodes:") (newline)
    (display (format #f "(length subnodes): ~A"
                     (length subnodes)))
    (newline)

    ;; (display (format #f "(ast :subnodes): ~A" (ast :subnodes)))
    ;; (newline)

    ;; (let ((nd (ast-nodelist-ref subnodes 0)))
    ;;   (display (format #f "subnode 0 type: ~A ~A"
    ;;                    (nd :tid)
    ;;                    (tokens (nd :tid))))
    ;;   (newline))

    (for-each
         (lambda (node)
           (display (format #f "\tnode type: ~A" (node :tid)))
           (newline))
         subnodes)

    ))

(define (test-walk ast)
  (walk-all ast handlers)
  (newline)
  )

(define (test-property-lookup ast)
  ;; field lookup
  (display (format #f "(ast :tid): ~A" (ast :tid))) (newline)
  (display (format #f "(ast :line): ~A" (ast :line))) (newline)
  (display (format #f "(ast :col): ~A" (ast :col))) (newline)

  ;; token type predicates: token type keyword plus '?'
  ;; ERROR: :stmt_list? not found, did you mean :stmt-list?
  ;; (display (format #f "(ast :stmt_list?): ~A" (ast :stmt_list?)))
  ;; (newline)
  (display (format #f "(ast :stmt-list?): ~A" (ast :stmt-list?)))
  (newline)
  (display (format #f "(ast :stmt-list?): ~A" (ast :stmt-list?)))
  (newline)
  (display (format #f "(ast :stmt-list?): ~A" (ast :stmt-list?)))
  (newline)

  ;; pseudo-attrbutes
  (display (format #f "(ast :printable?): ~A" (ast :printable?)))
  (newline)
  (display (format #f "(ast :print): ~A" (ast :printable?)))
  (newline)
  )

(define (test-rule-attrs ast)
  ;; attrib lookup for :call_expr nodes
  (display (format #f "(ast :@deps): ~A" (ast :@deps)))
  (newline)
  )

(define (test-tokens-tables ast)
      ;; tokens table (vector):
  (display (object->string tokens))
  (newline)
    (display (object->string sunlark-tids))
    (newline)
    (display (sunlark-tids :list-expr))
    (newline)

    ;; (display (tokens 127)) ;; :stmt_list
    ;; (newline)

    ;; (display (format #f "ref help: ~A" (help ast-node-ref)))
    (newline)
  )

(define (test-subnodes ast)
  (display "................") (newline)
  (display (format #f "(ast-node? ast) ~A" (ast-node? ast))) (newline)

  ;; (ast :tid)
  ;; (display (format #f "(ast :tid) ~A" (ast :tid))) (newline)

  ;; (display ((ast :subnodes) :tid)) (newline)
  ;; (display (ast-node? (ast :subnodes))) (newline)
  ;; (display (ast-nodelist? (ast :subnodes))) (newline)
  ;; (display (tokens ((ast :subnodes) :tid))) (newline)  ;; :node-list

  ;; (let ((ast-subnodes (ast :subnodes)))
  ;;   (display (ast-node? ast-subnodes)) (newline)
  ;;   (display (ast-nodelist? ast-subnodes)) (newline)
  ;;   (display (ast-subnodes :tid)) (newline)
  ;;   (display (tokens (ast-subnodes :tid))) (newline)
  ;;   (display (format #f "ast-subnodes tid: ~D ~A "
  ;;                    (ast-subnodes :tid)
  ;;                    (tokens (ast-subnodes :tid))))
  ;;   (newline)
  ;;   (display (format #f "(ast-subnodes :line) ~A "
  ;;                    (ast-subnodes :line)))
  ;;            )
  ;;   (newline)

  (let ((nl (ast :subnodes)))
    (display (ast-nodelist? nl)) (newline)
    (display (format #f "(length nl) ~D "
                     (length nl)))
  ;;   ;; (newline)
  ;;   ;; (display (format #f "(ast-subnodes :line) ~A "
  ;;   ;;                  (ast-subnodes :line)))
             )
    (newline)
  )

(define (test-structure ast)
  ;;  :build-file > :stmt-list :smallstmt-list > :expr-list > call-expr
  (display "test-structure")
  (newline)

  (let ((nl (ast :subnodes)))
    (display (ast-nodelist? nl)) (newline)
    (display (format #f "(length nl) ~D "
                     (length nl))) (newline)
    ;; (display (format #f "(ast-nodelist-ref nl 0 0 1 0 0) ~A "
    ;;                  (tokens ((ast-nodelist-ref nl 0 0 1 0 0) :tid)))) (newline)

    ;; (display (format #f "(ast-nodelist-ref nl 0 0 1 0 0 :s): ~A "
    ;;                  (ast-nodelist-ref nl 0 0 1 0 0 :s)))
    ;; (newline)

    (display (format #f "(ast-nodelist-ref nl 0 0) ~A "
                     (tokens ((ast-nodelist-ref nl 0 0 0) :tid))))
    (newline)

    ;; (display (format #f "(ast-nodelist-ref nl :stmt-list): ~A "
    ;;                  (tokens ((ast-nodelist-ref
    ;;                            nl :stmt-list :smallstmt-list :expr-list
    ;;                            :call-expr 0) :tid))))
    ;; (newline)

    ;; (display (format #f "(nl 0 0 0) ~A "
    ;;                  (nl 0 0 0))) (newline)
                     )
  )

(define (test-package ast)
  ;;  :build-file > :stmt-list :smallstmt-list > :expr-list > :call-expr
  (display "test-structure")
  (newline)

  (let ((nl (ast :subnodes)))
    (display (ast-nodelist? nl)) (newline)
    (display (format #f "(length nl) ~D "
                     (length nl))) (newline)
    ;; (display (format #f "(ast-nodelist-ref nl 0 0 1 0 0) ~A "
    ;;                  (tokens ((ast-nodelist-ref nl 0 0 1 0 0) :tid)))) (newline)

    ;; (display (format #f "(ast-nodelist-ref nl 0 0 1 0 0 :s): ~A "
    ;;                  (ast-nodelist-ref nl 0 0 1 0 0 :s)))
    ;; (newline)

    (display (format #f "(ast-nodelist-ref nl 0 0) ~A "
                     (tokens ((ast-nodelist-ref nl 0 0 0) :tid))))
    (newline)

    ;; (display (format #f "(ast-nodelist-ref nl :stmt-list): ~A "
    ;;                  (tokens ((ast-nodelist-ref
    ;;                            nl :stmt-list :smallstmt-list :expr-list
    ;;                            :call-expr 0) :tid))))
    ;; (newline)

    ;; (display (format #f "(nl 0 0 0) ~A "
    ;;                  (nl 0 0 0))) (newline)
                     )
  )

(define (test-predicates ast)
  (display "test-predicates")
  (newline)

  ;; (display (format #f "(ast :build-file?) ~A" (ast :build-file?)))
  ;; (newline)

  ;; (display (format #f "((ast 0) :build-file?) ~A" ((ast 0) :build-file?)))
  ;; (newline)

  ;; (display (format #f "((ast 0) :stmt-list?) ~A" ((ast 0) :stmt-list?)))
  ;; (newline)

  ;; (display (format #f "((ast 0 0) :small-stmt-list?) ~A"
  ;;                  ((ast 0 0) :small-stmt-list?)))
  ;; (newline)

  ;; (display (format #f "((ast 0 0 0) :load-stmt?) ~A"
  ;;                  ((ast 0 0 0) :load-stmt?)))
  ;; (newline)

  ;; (display (format #f "(ast :tid->kw) ~A" (ast :tid->kw)))
  ;; (newline)

  ;; (display (format #f "(ast :tid->string) ~A" (ast :tid->string)))
  ;; (newline)

  ;; (display (format #f "(ast-node? ast) ~A" (ast-node? ast)))
  ;; (newline)

  ;; (display (format #f "(ast :node?) ~A" (ast :node?)))
  ;; (newline)

  (display (format #f "((ast :subnodes) :node?) ~A"
                   ((ast :subnodes) :node?)))
  (newline)
  (display (format #f "((ast :subnodes) :nodelist?) ~A"
                   ((ast :subnodes) :nodelist?)))
  (newline)

  ;; (display (format #f "(ast-nodelist? ast) ~A" (ast-nodelist? ast)))
  ;; (newline)
  )


(define (test-load-stmt ast)
  ;;  :build-file > :stmt-list :smallstmt-list > load-expr
  ;; load("@rules_cc//cc:defs.bzl", "cc_binary", "cc_library", "cc_test")
  ;; :load-stmt > :load > lparen > string > comma ... > rparen
  ;; multiple consecutive load stmts go in smallstmt-list

  ;; with aliasing:
  ;; :load-stmt > :load + :lparen + :string + comma + ...
  ;; ... + alias + comma + alias ...
  ;;  :alias > :id + :eq + :string

  (display "test-load-stmt")
  (newline)

  (let ((nl (ast :subnodes)))
    ;; (display (format #f "ast tid: ~D ~A"
    ;;                  (ast :tid) (tokens (ast :tid)))) (newline)
    ;; (display (format #f "tid: ~D ~A"
    ;;                  (nl :tid) (tokens (nl :tid)))) (newline)
    ;; (display (ast-nodelist? nl)) (newline)
    ;; (display (ast-node? nl)) (newline)

    ;; (display (format #f "(length nl) ~D "
    ;;                  (length nl))) (newline)

    ;; (display (format #f "(length ((ast-node-ref ast 0 0) :subnodes)) ~A "
    ;;                  (length ((ast-node-ref ast 0 0) :subnodes))))
    ;; (newline)

    ;; (display (format #f "(ast-node-ref ast 0 0 0) ~A "
    ;;                  (tokens ((ast-node-ref ast 0 0 0) :tid))))
    ;; (newline)

    ;; (display (format #f "(tokens (ast 0 0 0 :tid)) ~A "
    ;;                  (tokens (ast 0 0 0 :tid))))
    ;; (newline)

    ;; get load "key" - 3rd child of :load-stmt
    ;; (display (format #f "(ast 0 0 0 2 :s) ~A "
    ;;                  (ast 0 0 0 2 :s)))
    ;; (newline)

    ;; (display (format #f "(ast-node-ref ast 0 0 0 2 :s) ~A "
    ;;                  (ast-node-ref ast 0 0 0 2 :s)))
    ;; (newline)

    ;; get (Scheme) list of all load stmts
    ;; (display (format #f "(ast :loads) ~D:\n ~A "
    ;;                  (length (ast :loads))
    ;;                  (object->string (ast :loads) :readable)))
    ;; (newline)

    ;; (display (format #f "(ast :loads 1)\n ~A "
    ;;                  (object->string (ast :loads 1) :readable)))
    ;; (newline)

    ;; ;; index -1 == last
    ;; (display (format #f "(ast :loads -1)\n ~A "
    ;;                  (object->string (ast :loads -1) :readable)))
    ;; (newline)

    ;; (display (format #f "(ast :loads \"Bloader\")\n ~A "
    ;;                  (object->string (ast :loads "Bloader") :readable)))
    ;; (newline)

    (ast :loads ; "Bloader"
         (lambda (result)
           ;;(length result)
           (display "hello!")
           (newline)
           (display (result :node?))
           (newline)
           (if (result :node?)
               (begin
                 (display (result :tid->kw))
                 (newline)))
           (if (result :nodelist?)
               (begin
                 (for-each
                  (lambda (node)
                    (display (node :subnodes 2 :s))
                    (newline)
                    (set! (node :subnodes 2 :s) "Xloader")
                    (newline))
                  result)
                 ;; (for-each
                 ;;  (lambda (node)
                 ;;    (display (node :tid->kw))
                 ;;    (newline)
                 ;;    (display (node :subnodes 2))
                 ;;    (newline))
                 ;;  result)
                 (newline)))
           ))
    (newline)
    ))

(define (test-load-update ast)
  (display "test-load-update")
  (newline)
  (display (ast :loads "Bloader" :subnodes 2 :s))
  (newline)
  ;; this evals to a string (ast :loads "Bloader" :subnodes 2 :s)
  ;; so we cannot update it with
  ;; (set! (ast :loads "Bloader" :subnodes 2 :s) "FOO")
  ;; instead:
  (set! (ast :loads "Bloader" :subnodes 2) '(:replace :s "FOO"))
  (newline)

  ;; better api, hiding the gory details:
  ;; (ast :load "Bloader.bzl" :args) - string args
  ;; (ast :load "Bloader.bzl" :attrs) - name="val" pairs
  ;; compare targets
  ;; (ast :target "mylib" :id) - rule name, e.g. cc_library
  ;; (ast :target "mylib" :args) - string args
  ;; (ast :target "mylib" :attrs) - name="val" pairs
  ;; (ast :target "mylib" :attrs :name) - "name" attrib

  ;;
  (set! (ast :loads "Bloader.bzl") "Foo.bzl") (newline)

  ;; (set! (ast :load "Bloader.bzl" :args) '(:add "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:replace "a" "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:replace 0 "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:remove "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:remove 0)) (newline)

  ;; ;; remove arg from all load stmts:
  ;; (set! (ast :load :* :args) '(:remove "foo")) (newline)

  ;; ;; replace "foo" in all load stmts, ignoring those that do not have it:
  ;; (set! (ast :load :* :args) '(:replace "foo" "bar")) (newline)

  (display "................")
  (newline)
  ;; (display (ast :loads "Bloader" :subnodes 2 :s))
  ;; (newline)

  ;; (set! (deps 'name) "newdeps")
  ;;         ;; (set! (deps 'value) '(:replace "dep_a" "dep_X"))
  )

(define (pprint-node node)
  (display
   (format #f "~NC~A[~A] ~A~A"
           2 ;; indent
           #\space
           (tokens (node :tid))
           (node :tid)
           (if (node :printable?) "____    " "")
           (if (node :printable?) "(node :pprint)" "")))
  (newline)
  )

(define (test-target-update ast)
  (display "test-target-update")
  (newline)

  ;; (walk-all ast handlers)
  ;; (newline)

  ;; (display (object->string ast :readable))
  ;; (newline)

  ;; (display (length (ast :targets)))
  ;; (newline)

  ;; (display (object->string (ast :targets) :readable))
  ;; (newline)

  ;; (display (ast :target "hello-lib"))
  ;; (newline)

  ;; (display (ast :target "hello"))
  ;; (newline)

  ;; (display (ast :load "@rules_cc//cc:defs.bzl"))
  ;; (newline)
  ;; (display (ast :load "@rules_foo//foo:defs.bzl"))
  ;; (newline)

  (display (format #f "(ast :target ...):\n~A"
                   (object->string (ast :target "b-lib")
                                   :readable)))
  (newline)

  ;; (display (ast :target "hello" :attrs))
  ;; (newline)

  ;; (display (ast :target "hello" :attrs :deps))
  ;; (newline)

  ;;  by rule name, e.g. cc_library
  ;;  input: test/data/walk/BUILD.target3
  ;; (let* ((tlist (ast :target 'cc_library))
  ;;        (tlen (length tlist)))
  ;;   (display (format #f "(ast :target ...)\n~A"
  ;;                    (object->string
  ;;                     tlist
  ;;                     :readable)))
  ;;   (newline)
  ;;   (display (format #f "target count: ~D" tlen))
  ;;   (newline)
  ;;   )

  ;; by target id == value of name attr. :target "foo" means
  ;; filter the list of targets by attribute name="foo"
  ;;  result is one node

  ;; (let* ((tnode (ast :target "b-lib" :attrs))
  ;;        ;; tnode is a list node, not a list. attrs are its subnodes
  ;;        (tlen (length (tnode :subnodes))))
  ;;   (display (format #f "(ast :target ...)\n~A"
  ;;                    (object->string
  ;;                     tnode ;; a list node, not a list
  ;;                     :readable)))
  ;;   (newline)
  ;;   (display (format #f "attr count: ~D" tlen))
  ;;   (newline)
  ;;   )

  ;; returns :arg-named > :id :eq :list-expr > :lbrack, :expr-list, :rbrack
  ;;   :expr-list == :string, :comma ...
  ;; (let* ((tnode (ast :target "b-lib" :attrs 'srcs)))
  ;;   (display (format #f "(ast :target ...)\n~A"
  ;;                    (object->string
  ;;                     tnode
  ;;                     :readable)))
  ;;   (newline)
  ;;   )

  ;; value of 'srcs' attr is a string list, e.g. ["a", "b"]
  ;;  returns :list-expr > lbrack, :expr-list, :rbrack
  ;;    :expr-list == :string, :comma ...
  (let* ((tnode (ast :target "b-lib" :attrs 'srcs :value)))
    (display (format #f "(ast :target ...)\n~A"
                     (object->string
                      tnode
                      :readable)))
    (newline)
    )

  ;; (let* ((tlist (ast :target 'cc_test))
  ;;        (tlen (length tlist)))
  ;;   (display (format #f "(ast :target 'cc_library)\n~A"
  ;;                    (object->string tlist
  ;;                                    :readable)))
  ;;   (newline)
  ;;   (display (format #f "target count: ~D" tlen)))
  ;; (newline)

  ;; but :load <symbol> not allowed
  ;; (display (ast :load 'foo)) ;; meaning?


  ;; get list of srcs attrs for cc_library targets
  ;; (display (ast :target 'cc_library 'srcs))
  ;; (newline)

  ;; this evals to a string (ast :loads "Bloader" :subnodes 2 :s)
  ;; so we cannot update it with
  ;; (set! (ast :loads "Bloader" :subnodes 2 :s) "FOO")
  ;; instead:
  ;; (set! (ast :loads "Bloader" :subnodes 2) '(:replace :s "FOO"))
  ;; (newline)

  ;; better api, hiding the gory details:
  ;; (ast :load "Bloader.bzl" :args) - string args
  ;; (ast :load "Bloader.bzl" :attrs) - name="val" pairs
  ;; compare targets
  ;; (ast :target "mylib" :id) - rule name, e.g. cc_library
  ;; (ast :target "mylib" :args) - string args
  ;; (ast :target "mylib" :attrs) - name="val" pairs
  ;; (ast :target "mylib" :attrs :name) - "name" attrib

  ;;
  ;; (set! (ast :loads "Bloader.bzl") "Foo.bzl") (newline)

  ;; (set! (ast :load "Bloader.bzl" :args) '(:add "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:replace "a" "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:replace 0 "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:remove "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:remove 0)) (newline)

  ;; ;; remove arg from all load stmts:
  ;; (set! (ast :load :* :args) '(:remove "foo")) (newline)

  ;; ;; replace "foo" in all load stmts, ignoring those that do not have it:
  ;; (set! (ast :load :* :args) '(:replace "foo" "bar")) (newline)

  (display "................")
  (newline)
  ;; (display (ast :loads "Bloader" :subnodes 2 :s))
  ;; (newline)

  ;; (set! (deps 'name) "newdeps")
  ;;         ;; (set! (deps 'value) '(:replace "dep_a" "dep_X"))
  )

(define (test-user-properties ast)
  (display "test-user-properties")
  (newline)

  ;; cases:
  ;; (build-file-node :loads <load-key>)
  ;; (bfnode :targets <rulename>)
  ;; (attrs <attr-name>
  ;; other?

  ;; syntax options: sym, string, keyword, special prefix, reader macro
  ;; :$foo, ::foo, #$(foo), 'foo, :>foo, #>(foo)
  ;; possible initial chars in the real world: /, >, $, etc.

  ;; double-colon has the advantage of suggesting Bazel target syntax
  ;; but what if the string is ":foo", which will be common
  ;; then we would write :::foo?

  ;; (ast :loads :foo)
  ;; (ast :targets :footarget) vs. (ast :targets :cc_library)
  ;; (ast :targets #$(:foo))
  ;; uses cases commonality: always used as a kind of "key"

  ;; the only arg after :loads that makes sense is a key. For
  ;; :targets, the next arg could be a rule name or a target name. Or
  ;; it could be a function whose input is the output of :targets,
  ;; :loads etc.

  ;;  ::foo would match both "foo" and ":foo"

  (display (format #f "(ast :loads \"Aloader\")\n ~A "
                   (object->string (ast :loads "Aloader") :readable)))
  (newline)
  )

(define ast-handler
  (lambda (ast)
    (display (string-append "running ast-handler"))
    (newline)

    ;; (add-to-load-path "test/scm") ;; not supported
    (set! *load-path* (cons "test/scm" *load-path*))

    ;; (test-subnodes ast)

    ;; (test-structure ast)

    ;; (test-package ast)

    ;; (test-predicates ast)

    ;; (test-load-stmt ast)

    ;; (test-load-update ast)

    (test-target-update ast)

    ;; (display (object->string ast :readable))
    ;; (newline)

    ;; support for :subnodes? its an implementation detail e.g. (ast
    ;; :targets) must find the Call_Exprs, which are several levels
    ;; down, using :subnodes.

    ;; :subnodes complicates access-path resolution, since it returns
    ;; a nodelist not a node.

    ;; otoh, walking the tree is easy with :subnodes

    ;; (display (ast :targets :cc_library :attrs :deps)))

    ;; (test-walk ast)

    ;; (test-tokens-tables ast)

    ;; (test-property-lookup ast)

    ;; (test-subnodes ast)

    ;; (test-rule-attrs ast)

    ;; (display ast)
    ;; (newline)


    ;; ERROR: wrong type arg
    ;; (display (format #f "(ast 2): ~A" (ast 2)))
    ;; (newline)

    ;; (display (ast-node->starlark ast))
    ;; (newline)

    ;; (display (format #f "(ast :foo): ~A"
    ;;                  (ast :foo)))
    ;; (newline)

    ;; (let* ((p (open-input-file "test/data/cpp/BUILD.test"))
    ;;        (l (read-line p)))
    ;;   (close-input-port p)
    ;;   (display l)
    ;;   (newline))

    ;; (let* ((p (open-output-file "tmp/output.txt")))
    ;;   (display (ast-node->starlark ast) p)
    ;;   (newline p)
    ;;   (close-output-port p))

    ))
