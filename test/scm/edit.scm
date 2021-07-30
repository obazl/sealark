(display "hello from .sunlark.d/edit.scm")
(newline)

(load "ast_walk.scm")

(define handlers ()
  )

;; todo:

;; * list all target names (vals for key 'name') for a set of rule types
;; * for a globbed target name list the rule names, e.g.
;;     hello-world => cc_binary,  hello-lib => cc_library

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

  ;; pseudo-bindings
  (display (format #f "(ast :printable?): ~A" (ast :printable?)))
  (newline)
  (display (format #f "(ast :print): ~A" (ast :printable?)))
  (newline)
  )

(define (test-rule-bindings ast)
  ;; binding lookup for :call_expr nodes
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

  ;; (display (format #f "(node? ast) ~A" (node? ast)))
  ;; (newline)

  ;; (display (format #f "(ast :node?) ~A" (ast :node?)))
  ;; (newline)

  ;; (display (format #f "(node? (ast :subnodes)): ~A"
  ;;                  (node? (ast :subnodes))))  ;; #f
  ;; (newline)

  ;;;;;;;;
  ;; (display (format #f "(nodelist? (ast :subnodes)): ~A"
  ;;                  (nodelist? (ast :subnodes)))) ;; #t
  ;; (newline)

  ;; (display (format #f "((ast :subnodes) :nodelist?) ~A"
  ;;                  ((ast :subnodes) :nodelist?))) ;; #t
  ;; (newline)

  ;; (display (format #f "((ast :subnodes) :node?) ~A"
  ;;                  ((ast :subnodes) :node?))) ;; #f
  ;; (newline)

  ;;;;;;;;;;;;;;;;
  ;; (display (format #f "((ast :) ~A"
  ;;                  (list? (ast :subnodes))))
  ;; (newline)

  (display (format #f "((ast :targets \"hello-lib\") :node?) ~A"
                   (ast :target "hello-lib"))) ;; #t
  (newline)

  )


(define (test-load-stmt ast)
  ;; testfile: test/data/load/BUILD.args
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

  (let* (
         ;; (path '(:target "hello-world" :bindings))

         ;; (path '(:targets))
         ;; (path '(:loads))

         ;; (path '(:load "@foo-a//bar-a:baza.bzl"))
         ;; (path '(:load "@foo-b//bar-b:bazb.bzl"))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl"))
         ;; (path '(:load "@rules_cc//cc:defs.bzl"))

         ;; (path '(:loads 0))
         ;; (path '(:loads 1))
         ;; (path '(:loads 2))
         ;; (path '(:loads 3))

         ;; (path '(:load "@foo-a//bar-a:baza.bzl" :args))
         ;; (path '(:load "@foo-b//bar-b:bazb.bzl" :args))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :args))

         ;; (path '(:load "@foo-b//bar-b:bazb.bzl" :bindings))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings))

         ;; (path '(:load "@foo-b//bar-b:bazb.bzl" :arg-list))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list))

         ;; (path '(:loads 0 :args))
         ;; (path '(:loads 1 :args))
         ;; (path '(:loads 2 :args))

         ;; (path '(:loads 0 :args))
         ;; (path '(:loads 0 :bindings))

         ;;;; nope (path '(:loads :count))
         ;; (path '(:loads '("foo" "bar")))

         ;;;;  tetrads

         ;; (path '(:load "@foo-b//bar-b:bazb.bzl" :args 0))
         (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings 0))
         (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings 1))
         (path '(:loads 1 :bindings 0))

         ;; (path '(:targets))

         ;; (path '(:load "@foo-b//bar-b:bazb.bzl" :bindings 0))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings 0))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings 1))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings 2))

         ;; (path '(:load "@foo-b//bar-b:bazb.bzl" :arg-list 0))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list 0))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list 1))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list 2))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list 3))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list 4))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list 5))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :arg-list 6))

         ;; (path '(:load "@foo-c//bar-c:bazc.bzl"))

         ;;;; pentads

         (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings 0 :key))
         ;; (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings 0 :value))

         (path '(:load "@foo-c//bar-c:bazc.bzl" :bindings key1c :value))

         (path '(:loads 2 :bindings key1c :key)) ;; = key1c
         (path '(:loads 2 :bindings key1c :value))

         ;; (path '(:loads))


                                        ;        (path '(:load "@foo-b//bar-b:bazb.bzl"))
         ;; (path '(:load "foo" :args))
         ;; (path '(:load "foo" :bindings))

         ;; (path '(:loads "foo" :args)) ;; illegal
         )

    ;; (display ast)
    (display (format #f "(sunlark->starlark ... )\n\n~A"
                     (sunlark->starlark
                      ;;ast
                      (apply ast path)
                      :crush))) (newline)
    (newline)

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

    ;; (ast :loads ; "Bloader"
    ;;      (lambda (result)
    ;;        ;;(length result)
    ;;        (display "hello!")
    ;;        (newline)
    ;;        (display (result :node?))
    ;;        (newline)
    ;;        (if (result :node?)
    ;;            (begin
    ;;              (display (result :tid->kw))
    ;;              (newline)))
    ;;        (if (result :nodelist?)
    ;;            (begin
    ;;              (for-each
    ;;               (lambda (node)
    ;;                 (display (node :subnodes 2 :s))
    ;;                 (newline)
    ;;                 (set! (node :subnodes 2 :s) "Xloader")
    ;;                 (newline))
    ;;               result)
    ;;              ;; (for-each
    ;;              ;;  (lambda (node)
    ;;              ;;    (display (node :tid->kw))
    ;;              ;;    (newline)
    ;;              ;;    (display (node :subnodes 2))
    ;;              ;;    (newline))
    ;;              ;;  result)
    ;;              (newline)))
    ;;        ))
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
  ;; (ast :load "Bloader.bzl" :bindings) - name="val" pairs
  ;; compare targets
  ;; (ast :target "mylib" :id) - rule name, e.g. cc_library
  ;; (ast :target "mylib" :args) - string args
  ;; (ast :target "mylib" :bindings) - key="val" pairs
  ;; (ast :target "mylib" :bindings :foo) - binding with :key foo

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

  ;; (set! (deps 'key) "newdeps")
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

  ;; (display (object->string (ast :targets) :readable))
  ;; (newline)

  ;; (display (length (ast :targets)))
  ;; (newline)

  ;; (display (ast :target "hello-lib"))
  ;; (newline)

  ;; (display (ast :target "hello"))
  ;; (newline)

  ;; (display (ast :load "@rules_cc//cc:defs.bzl"))
  ;; (newline)
  ;; (display (ast :load "@rules_foo//foo:defs.bzl"))
  ;; (newline)

  ;; (display (format #f "(ast :target ...):\n~A"
  ;;                  (object->string (ast :target "b-lib")
  ;;                                  :readable)))
  ;; (newline)

  ;; (display (ast :target "hello-world" :bindings))
  ;; (newline)

  ;; (display (ast :target "hello" :bindings :deps))
  ;; (newline)

  ;;  by rule name, e.g. cc_library
  ;;  input: test/data/walk/BUILD.target3

  ;; (let* (
  ;;        (path '(:targets)); 'cc_test))
  ;;        ;; (tlen (length tlist))
  ;;        )
  ;;   (display (format #f "(sunlark->starlark ... )\n\n~A"
  ;;                    (sunlark->starlark
  ;;                     ;;ast
  ;;                     (apply ast path)
  ;;                     :crush))) (newline)
  ;;   ;; (display (format #f "(ast :target ...)\n~A"
  ;;   ;;                  ;; (object->string tlist :readable)
  ;;   ;;                  ;; (sunlark->starlark tlist)
  ;;   ;;                  ;; (sunlark->starlark tlist :squeeze)
  ;;   ;;                  (sunlark->starlark tlist :crush)
  ;;   ;;                  ))
  ;;   (newline)
    ;; (display (format #f "target count: ~D" tlen))
    ;; (newline)
    ;; )

  ;; by target id == value of name binding. :target "foo" means
  ;; filter the list of targets by binding name="foo"
  ;;  result is one node

  ;; (let* ((tnode (ast :target "b-lib" :bindings))
  ;;        ;; tnode is a list node, not a list. bindings are its subnodes
  ;;        (tlen (length (tnode :subnodes))))
  ;;   (display (format #f "(ast :target ...)\n~A"
  ;;                    (object->string
  ;;                     tnode ;; a list node, not a list
  ;;                     :readable)))
  ;;   (newline)
  ;;   (display (format #f "binding count: ~D" tlen))
  ;;   (newline)
  ;;   )

  ;; returns :arg-named > :id :eq :list-expr > :lbrack, :expr-list, :rbrack
  ;;   :expr-list == :string, :comma ...
  ;; (let* ((tnode (ast :target "b-lib" :bindings 'srcs)))
  ;;   (display (format #f "(ast :target ...)\n~A"
  ;;                    (object->string
  ;;                     tnode
  ;;                     :readable)))
  ;;   (newline)
  ;;   )

  ;; value of 'srcs' binding is a string list, e.g. ["a", "b"]
  ;;  returns :list-expr > lbrack, :expr-list, :rbrack
  ;;    :expr-list == :string, :comma ...
  ;; (let* ((tnode (ast :target "b-lib" :bindings 'srcs :value)))
  ;;   (display (format #f "(ast :target ...)\n~A"
  ;;                    (object->string
  ;;                     tnode
  ;;                     :readable)))
  ;;   (newline)
  ;;   )

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
  )

  ;; get list of srcs bindings for cc_library targets
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
  ;; (ast :load "Bloader.bzl" :bindings) - key="val" pairs
  ;; compare targets
  ;; (ast :target "mylib" :rule - rule name, e.g. cc_library
  ;; (ast :target "mylib" :args) - string args
  ;; (ast :target "mylib" :bindings) - key="val" pairs
  ;; (ast :target "mylib" :bindings 'foo) - "foo" binding

(define (test-set ast)
  (display "test-target-update")
  (newline)

  ;; (set! (ast :load "Bloader.bzl") "Foo.bzl") (newline)
  (set! (ast :load "Bloader.bzl" :args) '(:replace "a" "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:replace 0 "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:remove "foo")) (newline)
  ;; (set! (ast :load "Bloader.bzl" :args) '(:remove 0)) (newline)

  (let (
        (path '(:load "Bloader.bzl" :args))
        )
    (display (format #f "(sunlark->starlark ... )\n\n~A"
                     (sunlark->starlark
                      ast
                      ;;(apply ast path)
                      :crush)))
    (newline)
    )

  ;; ;; remove arg from all load stmts:
  ;; (set! (ast :load :* :args) '(:remove "foo")) (newline)

  ;; ;; replace "foo" in all load stmts, ignoring those that do not have it:
  ;; (set! (ast :load :* :args) '(:replace "foo" "bar")) (newline)

  (display "................")
  (newline)
  ;; (display (ast :loads "Bloader" :subnodes 2 :s))
  ;; (newline)

  ;; (set! (deps 'key) "newdeps")
  ;;         ;; (set! (deps 'value) '(:replace "dep_a" "dep_X"))
  )

(define (test-vector-attrs ast)
  (display "test-vector-attrs")
  (newline)
  (let* (
         ;; (svec (ast :target "hello-lib" :bindings srcs :value))

         ;; (svec (bindings 'srcs :value))
        ;; (path '(:targets 0 :bindings srcs))

        ;; indexing works for char values too?
        ;; mm, no - return val would not be a node in the tree
        ;; use scheme to do that
        ;; (path '(:targets 0 :bindings srcs :value (0 2)))

        ;; (add! (ast :targets 0 :bindings srcs :value) "foo")
        ;; (add! (ast :targets 0 :bindings srcs :value) ("foo" "bar"))
        ;; (set! (ast :targets 0 :bindings srcs :value) ("foo" "bar"))
        ;; (set! (ast :targets 0 :bindings srcs :value 0) "foo")

        ;; convert ["a", "b", "c"] to ["a", ["foo" "bar"], "c"]
        ;; not valid for targets, but ok for e.g. variables
        ;; but for var decls, might as well just use Scheme directly
        ;; (set! (ast :targets 0 :bindings srcs :value 1) ("foo" "bar"))

        ;; (path '(:targets 0 :bindings srcs :value 0))
         (bindings (ast :target "string-vectors" :bindings))
         ;; (svec (bindings 1 :value))
         (binding1 (bindings 2))
         (vec1 (binding1 :value))
         (item1 (vec1 1))
        )
    ;; (set! (vec1 1) "foo")
    ;;(ast-node-set! item1 :s "bar")
    ;; (set! (item1) "bar")
    ;; (set! (binding1 :key) "barrrrrrrrrrrrrrrrr")
    ;; (set! (binding1 :value) "bzzzz")

    ;; invalid
    ;; (set! (binding1) "foo") ;; newval must be pair
    ;; (set! (binding1) '(foo (1 "baz"))) ;; heterogenous list val
    ;; (set! (binding1) '(foo (#t #t #f))) ;; wrong elt type
    ;; (set! (binding1) '(:foo (1 2))) ;; bad key

    ;; ok: car is sym or string;
    ;; cadr: bool, string/sym, or int; or list of ints or string/sym
    ;; (set! (binding1) '(foo . "bar"))
    ;; (set! (binding1) '(foo #t))
    ;; (set! (binding1) '(foo "bar"))
    ;; (set! (binding1) '(foo bar)) ;; sym => variable
    ;; (set! (binding1) '(foo 3))
    ;; ;; lists
    (set! (binding1) '(string-foo ("bar" "baz")))
    ;; (set! (binding1) '(foo ("bar" myvar "baz")))
    ;; (set! (binding1) '(foo (1 2 3)))
    ;; (set! (binding1) '(foo (1 myvar 3)))

    (display (format #f "(sunlark->starlark ... )\n~A"
                     ;; (item1 :s)
                     ;; svec
                     ;;item1

                     ;; binding1
                     (sunlark->starlark ast :crush)

                     ;; (object->string ast)
                     ;; (object->string (apply ast path))
                     ;; (sunlark->starlark (apply ast path) :crush)
                     ))
    (newline)
    )
 )

(define (test-int-vectors ast)
  (display "test-int-vectors")
  (newline)
  (let* (
         ;; (vec (sunlark-parse-string "[1, 2, 3]"))
        ;; (path '(:targets 0 :bindings srcs :value 0))
         ;; (bindings (ast :target "int-vectors" :bindings))
         ;; (svec (bindings 1 :value))
         ;; (binding1 (bindings "int_veca")) ;; no, bindings is a scheme list
         ;; (vec1 (binding1 :value))
         ;; (item1 (vec1 1))

         ;; (vec (ast :target "string-vectors" :binding 1 :value))

         ;;(v (v "string-vectors"))
         ;; (v (v 0))

         ;; (v (v :@))
         ;; (v (v 'string_vecc))
         ;; (v (v :value))

         ;;;; now new item ct > old item ct
         ;; (result (set! (vec) '(31 32 33 34)))

         ;; (result (set! (vec 1) "foo"))
         ;; (v (map (lambda (x)
         ;;           ;;(display (format #f "~A" (sunlark->starlark x :crush)))
         ;;           ;;(newline))
         ;;           (set! (x :value) '("foo" "bar" "baz" "asfdasdf"))
         ;;           )
         ;;         v))
        )
    ;; (set! (vec) '())
    ;; (set! (binding1) '(foo ("bar" myvar "baz")))
    ;; (set! (binding1) '(foo (1 2 3)))
    ;; (set! (binding1) '(foo (1 myvar 3)))
    ;; (display (format #f "set! result: ~A" result)) (newline)
    ;; (display v) (newline)
    (display (format #f "(sunlark->starlark ... )\n~A"
                     ;; newvec
                     ;; (sunlark-node? v)
                     ;; v
                     ;; (list? v)
                     (sunlark->starlark v :crush)
                     ;; (sunlark->starlark (list? vec) :crush)
                     ;; (item1 :s)
                     ;; svec
                     ;;item1

                     ;; (object->string ast)
                     ;; (object->string (apply ast path))
                     ;; (sunlark->starlark (apply ast path) :crush)
                     ))
    (newline)
    )
 )

(define (test-set-vector ast)
  (display "test-set-vector")
  (newline)
  (let* (
         ;;(v (sunlark-parse-string "[1, 2, 3]"))
         (v (sunlark-parse-string "['a', 'b', 'c']"))
         ;; (x (set! (v 1) 'x))

         (s (sunlark-make-string "foo" :qtype :rawbin :qqq 3))

         (x (set! (v 1) s))

         ;; (bf "test/unit/sunlark/BUILD.vectors")
         ;; (ast (sunlark-parse-build-file bf))
         ;; (path '(:> 1 :@ 1)) ;; int_veca = [1, 2, 3]
         ;; (v (apply ast path))

         ;;(v (length v)) ;; 3
         ;; (v (v :subnodes))
         ;; (result (set! (vec) '(31 32 33 34)))

         ;; (result (set! (vec 1) "foo"))
        )
    ;; (for-each (lambda (x)
    ;;                     (display (format #f "~A" x))
    ;;                     (newline)
    ;;                ;;(set! (x :value) '("foo" "bar" "baz" "asfdasdf"))
    ;;                )
    ;;              v)
    ;; (set! (vec) '())
    ;; (display (sunlark-debug-print v)) (newline)
    ;; (display ast) (newline)
    (display (format #f "(sunlark->starlark ... )\n~A"
                     ;s
                     (sunlark->starlark v :crush)
                     ;; (object->string (apply ast path))
                     ;; (sunlark->starlark (apply ast path) :crush)
                     ))
    (newline)
    )
 )

(define (test-strings ast)
  (display "test-strings")
  (newline)
  (let* (
         ;;(v (sunlark-parse-string "[1, 2, 3]"))
         ;; (v (sunlark-parse-string "['a', r'b', 'c']"))
         ;; (v (v 1))
         ;; (x (set! (v 1) 'x))

                                 ;; :qtype :rawbin :qqq 3))
         (pdq1 (sunlark-make-string "I am a plain dq 1 string"))
         ;; (psq1 (sunlark-make-string "I am a plain sq 1 string"
         ;;                            :type :binary :q #\' :qqq #t))
         (hi (sunlark-make-string "hello" :q #\' :type :raw :qqq #t))
        )
    ;; (set! (pdq1) "howdy")
    (display pdq1) (newline)
    (set! (pdq1) hi)
    ;; (display (sunlark-debug-print v)) (newline)
    (display pdq1) (newline)
    (display (format #f "(sunlark->starlark ... )\n~A"
                     ;;pdq1 ;; :$)
                     ;; (psq1 :$)
                     (sunlark->starlark pdq1 :crush)
                     ;; (object->string (apply ast path))
                     ;; (sunlark->starlark (apply ast path) :crush)
                     ))
    (newline)
    )
 )

(define (test-directives ast)
  (display "test-directives")
  (newline)
  (let* (
        (path '(:directives))
        ;; (path '(:targets))
        ;; (path '(:loads))
        ;; (path '(:package))
        ;; (path '(:definitions))
        ;; (path '(:vardefs))
        ;; (path '(:procedures))
        )
    (display (format #f "(sunlark->starlark ... )\n~A"
                     ;; (object->string ast)
                     ;; (object->string (apply ast path))
                     (sunlark->starlark (apply ast path) :crush)
                     ;; (sunlark->starlark ast :crush)
                     ))
    (newline)
    )
 )

(define (filter predicate? sequence)
  (display "FILTERING ...") (newline)
  (display (format #f "FILTER over seq: ~D ~D ~A"
                   (sequence :tid)
                   (length sequence)
                   (sequence :tid->string)))
  (newline)
  (display (format #f "ITEM 2 ~A"
                   (sunlark->starlark (sequence 4) :squeeze)
                   ))
  (newline)
  (sequence 0))
  ;; (newline)
  ;; (let loop ((seq sequence)
  ;;            (accum '()))
  ;;   ;; (if (null? seq)
  ;;   ;;     accum
  ;;   ;;     (if (predicate? (seq 0))
  ;;   ;;         (loop (cdr seq) (cons (seq 0) accum))
  ;;   ;;         (loop (cdr seq) accum)))
  ;;   (nodelist-ref sequence 3)))

(define (ruleset rules)
  (display "RULESET>>>") (newline)
  (lambda (targets)
    (display (format #f "LAMBDA over ~D ~A"
                     (targets :tid)
                     (targets :tid->string)))
    (newline)
    ;; filter targets by rules
    (filter (lambda (target) (memq (target :rule) rules))
            targets)))

(define (test-rulesets ast)
  (display "test-rulesets")
  (newline)

  ;; (walk-all ast handlers)
  ;; (newline)

  ;; (display (object->string ast :readable))
  ;; (newline)

  ;; (let* ((tlist (ast :target (ruleset '(cc_binary cc_library))))

  ;; (let* ((tnode (ast :target "hello-world"))
  (let* ((tlist (ast :targets (ruleset '(cc_binary cc_library))))
  ;; (let* ((tlist (ast :targets "hello-world"))
         ;; (tlen (length tlist))
         )
    (display (format #f "(ast :target ...)\n~A"
                     ;; tnode
                     ;; (tnode :rule)
                     ;; (object->string tlist :readable)
                     ;; (sunlark->starlark tlist)
                     (sunlark->starlark tlist :squeeze)
                     ;; (sunlark->starlark tlist :squeeze)
                     ;; (sunlark->starlark tlist :crush)
                     ))
    (newline)
    ;; (display (format #f "target count: ~D" tlen))
    (newline)
    )


  (display "................")
  (newline)
  )

(define (test-lists ast)
  (display "test-lists")
  (newline)

  ;; (ast :target "b-lib" :bindings 'srcs :value)

  (let* (
         ;; (tlist (ast :targets))
         ;; ;; (tcar (car tlist))
         ;; (tnode3 (tlist 3))
         ;; (targlist (tnode3 :arg-list)) ;; bindings list, including comma
         ;; (tbindings (tnode3 :bindings)) ;; bindings only, w/o commas

         (v (ast :> "hello-world" :@ 'srcs))
         )
    (display v)
    (newline)
    ;; (display (format #f "(sunlark->starlark tbindings*) ~A"
    ;;                  (sunlark->starlark
    ;;                   v
    ;;                   :crush))) (newline)

    ;; (display (object->string tbindings :readable))
    ;; (display (sunlark->starlark (cddr tlist) :crush))
    ;; (display (sunlark->starlark atts :crush))
    ;; (display (car tlist))
    (newline)
  ))

(define (test-targets-filter ast)
  (display "test-targets-filter")
  (newline)

  (let* (
         ;; (tlist (ast :targets))

         ;;;;  multiple filters not supported by DSL, but programmable
         ;;;; (tlist (ast :targets '(cc_binary 3 file*) '(cc_*))); No


         ;;;; symbols filter by target rule
         ;; (tlist (ast :targets '(cc_binary cc_library)))
         (tlist (ast :targets '(cc_library file*)))
         ;; (tlist (ast :targets 'cc_*))

         ;;;; strings filter by target name
         ;; (tlist (ast :targets "hello-lib"))
         ;; (tlist (ast :targets '("hello-world" "hello-lib")))

         ;;;; integers, by index
         ;; (tlist (ast :targets 0))
         ;; (tlist (ast :targets '(0)))
         ;; (tlist (ast :targets '(0 1)))
         ;; (tlist (ast :targets '(0 4)))
         ;; currently out-of-bounds indices in a list are not caught
         ;; (tlist (ast :targets '(-1 5)))

         ;;;; mixed
         ;; (tlist (ast :targets '(file* "hello-world")))
         ;; (tlist (ast :targets '("hello-world" cc_library)))
         ;; (tlist (ast :targets '("hello-world" file*)))
         ;; (tlist (ast :targets '("hello-world" 4)))
         ;; (tlist (ast :targets '(1 "srcs")))
         ;; (tlist (ast :targets '("srcs" 1))) ;; filter order doesn't matter
         ;; (tlist (ast :targets '(1 file*)))
         ;; (tlist (ast :targets '(cc_binary 3)))
         ;; (tlist (ast :targets '(cc_binary 1))) ;; both refer to same target
         )
    ;; (display (length tlist))
    ;; (newline)

    (display (format #f "(sunlark->starlark ... ) ~A"
                     (sunlark->starlark tlist :crush))) (newline)
    (newline)
  ))

(define (test-triads ast)
  (display "test-triads")
  (newline)

  (let* (
         ;; (tlist (ast :targets))

         ;; (tlist (ast :targets 1 :bindings))
         ;; (tlist (ast :targets 1 :arg-list))

         ;; (tlist (ast :target "hello-world" :arg-list))
         ;; (tlist (ast :target "hello-world" :bindings))

         ;;;; nope: (tlist (ast :target 1 :bindings)) ;; cannot index :target

         ;; filter then select
         (tlist (ast :targets '(cc_*) 0))
         ;; (tlist (ast :targets 'cc_test :count)) ;; not supported
         ;; (tlist (ast :targets '(cc_test f*)))
         ;; (tlist (ast :targets '(cc_test foo*) :count))
         ;; (tlist (ast :targets '(0 4) 0)) ;; nope

         ;; indexing filtered list
         ;; (tlist (ast :targets 0 :bindings))
         ;; error:
         ;; (tlist (ast :targets '(cc_binary cc_library) :bindings))

         ;; (tlist (ast :targets 0 :rule))
         ;; (tlist (ast :target "hello-lib" :rule))

        ;; (tlist (ast :loads 0 :key))
         ;; (tlist (ast :loads 0 :args))
         ;; (tlist (ast :loads 0 :bindings))

         ;; (tlist (ast :load "@rules_cc//cc:defs.bzl" :args))
         ;; (tlist (ast :load "@rules_cc//cc:defs.bzl" :bindings))

         )
    ;; (display (length tlist))
    ;; (newline)

    ;; (display (format #f "tlist:\n~A" tlist)) (newline)
    (display (format #f "tlist:~A"
                     (sunlark->starlark tlist :crush))) (newline)
  ))

(define (test-tetrads ast)
  (display "test-tetrads")
  (newline)

  (let* (
         ;; (tlist (ast :targets))

         ;; (tlist (ast :target "hello-world" :bindings))

         ;; (tlist (ast :target "hello-world" :bindings 'name))
         ;; (tlist (ast :target "hello-world" :bindings 'srcs))
         ;; (tlist (ast :target "hello-world" :bindings 'deps))
         ;;;; (tlist (ast :target "hello-world" :bindings 'fake))

         ;; (tlist (ast :target "hello-world" :bindings 0))
         ;; (tlist (ast :target "hello-world" :bindings 1))
         ;; (tlist (ast :target "hello-world" :bindings 2))
         ;;;; (tlist (ast :target "hello-world" :bindings 99)) ;; idx too big

         ;; (tlist (ast :targets 0 :bindings 'name))
         ;; (tlist (ast :targets 0 :bindings 'srcs))
         ;; (tlist (ast :targets 0 :bindings 'deps))

         ;; (tlist (ast :targets 0 :bindings 0))
         ;; (tlist (ast :targets 0 :bindings 1))
         ;; (tlist (ast :targets 0 :bindings 2))

         ;; (tlist (ast :targets 'cc_test 1 :bindings))
         ;; (tlist (ast :targets 'cc_* 0 :bindings))

         ;; (tlist (ast :targets '(cc_test file*) 0 :bindings))
         ;; (tlist (ast :targets '(cc_test file*) 1 :bindings))
         ;; (tlist (ast :targets '(cc_test file*) 2 :bindings))

         )
    ;; (display (length tlist))
    ;; (newline)

    (display (format #f "(sunlark->starlark ... ) ~A"
                     (sunlark->starlark tlist :crush))) (newline)
    (newline)
  ))

(define (test-pentads ast)
  (display "test-pentads")
  (newline)

  (let* (
         ;; (tlist (ast :targets))

         ;; (path '(:target "hello-world" :bindings))

         ;; NB: omit ' for symbols in quoted list

         ;; (path '(:target "hello-world" :bindings name))

         (path '(:target "hello-world" :bindings srcs :key))
         ;; (path '(:target "hello-world" :bindings srcs :value))

         ;; (tlist (ast :target "hello-world" :bindings 'deps))
         ;;;; (tlist (ast :target "hello-world" :bindings 'fake))

         ;; (path '(:target "hello-world" :bindings 0 :key))
         ;; (path '(:target "hello-world" :bindings 0 :value))
         ;; (tlist (ast :target "hello-world" :bindings 1))
         ;; (tlist (ast :target "hello-world" :bindings 2))
         ;;;; (tlist (ast :target "hello-world" :bindings 99)) ;; idx too big

         ;; (path '(:target "hello-world" :arg-list 0 :key))
         ;; (path '(:target "hello-world" :arg-list 0 :value))

         ;;;;  :targets

         ;; (path '(:targets 0 :bindings name))
         ;; (path '(:targets 0 :bindings srcs :key))
         ;; (path '(:targets 0 :bindings srcs :value))
         ;; (path '(:targets 0 :bindings 'deps :key))

         (path '(:targets 0 :bindings 0 :key))
         ;; (path '(:targets 0 :bindings 0 :value))
         ;; (path '(:targets 0 :bindings 1 :key))

         ;; (path '(:targets 0 :bindings 0 :key))
         ;; (tlist (ast :targets 0 :bindings 1))
         ;; (tlist (ast :targets 0 :bindings 2))

         ;; (path '(:targets 'cc_* 0 :bindings 0))
         ;; (path  '(:targets 'cc_test 1 :bindings srcs))

         ;; (path '(:targets '(cc_test file*) 0 :bindings srcs))
         ;; (path '(:targets '(cc_test file*) 0 :bindings 1))

         ;;;; loads

         ;; tetrads:
         ;; (path '(:loads 0 :args 0)) ;; key arg
         ;; (path '(:load "@rules_cc//cc:defs.bzl" :args 0)) ;; key arg

         ;; (path '(:loads 0 :bindings 0 :key))
         ;; (path '(:loads 2 :bindings 0 :value))

         ;; (path '(:loads 0 :bindings foo :key))
         ;; (path '(:loads 0 :bindings foo :value))

         ;; (path '(:loads ("foo" "bar") :bindings 0 :key))


         ;;;; error: :loads + string
         ;; (path '(:loads "@rules_cc//cc:defs.bzl" :arg-list 0 :key))

;         (path '(:load "@rules_cc//cc:defs.bzl" :arg-list 0 :key))

         ;; (path '(:load "@rules_cc//cc:defs.bzl" :bindings 0 :key))
         ;; (path '(:load "@rules_cc//cc:defs.bzl" :bindings 0 :value))

         ;; (path '(:load "@rules_cc//cc:defs.bzl" :bindings foo :key))
         ;; (path '(:load "@rules_cc//cc:defs.bzl" :bindings foo :value))

         )
    ;; (display (length tlist))
    ;; (newline)

    (display (format #f "(sunlark->starlark ... ) ~A"
                     (sunlark->starlark
                      (apply ast path)
                      :crush))) (newline)
    (newline)
  ))

(define (test-apply ast)
  (display "test-apply")
  (newline)

  (let* (
         ;; (tlist (ast :targets))

         (path '(:target "hello-world" :bindings 0))
         (tlist (apply ast path))

         ;; (tlist (ast :target "hello-world" :bindings 'name))
         ;; (tlist (ast :target "hello-world" :bindings 'srcs))
         ;; (tlist (ast :target "hello-world" :bindings 'deps))
         ;;;; (tlist (ast :target "hello-world" :bindings 'fake))

         ;; (tlist (ast :target "hello-world" :bindings 0))
         ;; (tlist (ast :target "hello-world" :bindings 1))
         ;; (tlist (ast :target "hello-world" :bindings 2))
         ;;;; (tlist (ast :target "hello-world" :bindings 99)) ;; idx too big

         ;; (tlist (ast :targets 0 :bindings 'name))
         ;; (tlist (ast :targets 0 :bindings 'srcs))
         ;; (tlist (ast :targets 0 :bindings 'deps))

         ;; (tlist (ast :targets 0 :bindings 0))
         ;; (tlist (ast :targets 0 :bindings 1))
         ;; (tlist (ast :targets 0 :bindings 2))

         ;; (tlist (ast :targets 'cc_test 1 :bindings))
         ;; (tlist (ast :targets 'cc_* 0 :bindings))

         ;; (tlist (ast :targets '(cc_test file*) 0 :bindings))
         ;; (tlist (ast :targets '(cc_test file*) 1 :bindings))
         ;; (tlist (ast :targets '(cc_test file*) 2 :bindings))

         )
    ;; (display (length tlist))
    ;; (newline)

    (display (format #f "(sunlark->starlark ... ) ~A"
                     (sunlark->starlark tlist :crush))) (newline)
    (newline)
  ))

(define (test-user-properties ast)
  (display "test-user-properties")
  (newline)

  ;; cases:
  ;; (build-file-node :loads <load-key>)
  ;; (bfnode :targets <rulename>)
  ;; (bindings <binding-name>
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

(define (test-fragments)
  (let ((node (parse-string
               "cc_library(a,b=['c', 'd'])")))
    (display node))
  (newline)
  ;; (let ((node (parse-build-file "test/data/cpp/BUILD.test")))
  ;;   (display node))
  ;; (newline)
  )

(define (test-bindings ast)
  (let* (
         (binding (ast :> 1 :@ 'srcs)) ;;(apply ast path))
         (k (binding :value))
         )

    (display (format #f "(sunlark->starlark ... )\n~A"
                     ((k 0) :print)
                     ;; (sunlark->starlark k :crush)
                     ;; (sunlark->starlark (binding :value) :crush)
                     ))
    (newline)
    ))

(define (test-dicts ast)
  ;; test file: test/unit/BUILD.dicts
  (let* (
         (d (ast :> "stringdict")); :@ 'adict)) ;;(apply ast path))
         ;; (d (d :@ 'adict :value 0 :key))
         ;; (d (d :@ 'adict :value 0 :value))

         ;; (d (d :@ 'adict :value '(:key "akey1"))) ; => :dict-entry
         ;; (d (d :@ 'adict :value '(:value "aval1"))) ; => :dict-entry

         ;; (d (d :@ 'adict :value '(:key "akey1") :value))
         (d (d :@ 'adict :value '(:value "aval1") :key))

         ;(d (d :value "akey1"))
         )

    (display (format #f "(sunlark->starlark ... )\n~A"
                     ;;d
                     (sunlark->starlark d :crush)
                     ;; (sunlark->starlark (binding :value) :crush)
                     ))
    (newline)
    ))

(define (test-targets ast)
  (let* (
         ;; (path '(:targets 0 :bindings srcs :value))
         ;; (path '(:targets 1))
         ;; (v (ast :> "string-vectors" :@ 'string_vecb :value))

         ;;(v (ast :>>))
         ;;(v (ast :>> :@)) ;; error
         ;; (v (ast :>> :name)) ;; error
         ;;(v (ast :>> :@@)) ;; all attrs of all targets (s7 list)

         ;;(v (ast :> 1))
         ;;(v (ast "hello-lib")) ;; error
         ;; (v (ast :> "hello-lib"))

         ;; (v (ast :> 1 :@@))
         ;; (v (ast :> 1 :@)) ;; error
         ;; (v (ast :> 1 :@ 'deps))
         ;; (v (ast :> 1 :@ 2))

         ;;(v (ast :> "vectors" :@ 'string_vec :value 1))
         ;;(v (ast :> "vectors" :@ 1 :value 1))

         ;; (v (ast :> "vectors"))
         ;; (v (v :@ 'string_vec :value 0))
         ;; (v (v :@ 1 :value 0))

         ;; (v (ast :> "vectors" :@ 'string_vec))
         ;; (v (ast :> "vectors" :@ 1))
         ;; (v (v :value 1))

         (v (ast :> "hello-lib" :@ 'srcs :key)) ; 'deps :value))
         ;;(v (v :print))
         ;; (v (ast :> 1 :@ 'deps :val)) ;; error
         ;; (v (ast :> 1 :@ 'deps 0)) ;; error

         ;; (v (ast :> 2 :@ 'string_vec :value 1))

         ;; (v (ast :>> :@@ :keys))

         ;; (vm (map (lambda (x)
         ;;            (display (format #f "~A" (sunlark->starlark x :crush)))
         ;;            (newline)
         ;;            (x :key)
         ;;            ;;98
         ;;           )
         ;;         v))
         )
    ;; (for-each (lambda (x)
    ;;             (display (format #f "x\n~A\n"
    ;;                              x)))
    ;;           ;; (sunlark-binding? x))))
    ;;           v)
    ;; (display  v) (newline)
    ;; (display (format #f "~A" (sunlark->starlark vm :crush))) (newline)
    (display (format #f "(sunlark->starlark ... )\n~A"
                     (sunlark->starlark
                      ;;ast
                      v ;; :key) ;;(v :subnodes)
                      ;;(cdr v)
                      ;; (length v)
                      ;; (sunlark-node? v)
                      ;;(t :name)
                      ;;(apply ast path)
                      :crush)))
    (newline)
    ))

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

    ;; (test-target-update ast)

    ;; (test-set ast)

    ;; (test-vector-attrs ast)

    ;; (test-int-vectors ast)

    ;; (test-set-vector ast)

    (test-strings ast)

    ;; (test-directives ast)

    ;; (test-rulesets ast)

    ;; (test-lists ast)

    ;; (test-dicts ast)

    ;; (test-targets ast)

    ;; (test-targets-filter ast)

    ;; (test-triads ast)

    ;; (test-tetrads ast)

    ;; (test-pentads ast)

    ;; (test-apply ast)

    ;; (test-bindings ast)

    ;; (test-fragments)

    ;; (display (object->string ast :readable))
    ;; (newline)

    ;; support for :subnodes? its an implementation detail e.g. (ast
    ;; :targets) must find the Call_Exprs, which are several levels
    ;; down, using :subnodes.

    ;; :subnodes complicates access-path resolution, since it returns
    ;; a nodelist not a node.

    ;; otoh, walking the tree is easy with :subnodes

    ;; (display (ast :targets :cc_library :bindings :deps)))

    ;; (test-walk ast)

    ;; (test-tokens-tables ast)

    ;; (test-property-lookup ast)

    ;; (test-subnodes ast)

    ;; (test-rule-bindings ast)

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

    ;; (display (test '#t))
    ;; (newline)

    ;; (display (test-report                   ;; Launch reporter for tests
    ;;  (test '(equal? 'foo 'foo))   ; passes
    ;;  (test '(my-foo arg1 arg2))   ; passes
    ;;  (test '(equal? 'foo 'bar))))
    ;; (newline)
    ))
