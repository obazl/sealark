(display "hello from test/scm/ast_walk.scm")
(newline)

;; handler: match predicate, edit selector, action
;; match predicate: selects one node, but may have multiple predicate; e.g.
;;   select cc_library rules with foo attrib. which node do we want to edit?
;; edit selector: selects the node to edit
;; (define-handler ...
;;   :match (:rule :cc_library :attr :deps) ;; select cc_library rules with deps
;;   :edit :attr-val ;; select attr-val of deps attr for editing
;;   :action (lambda (node) ... munge node ...)
;;   )
;; ;; result of define-handler:
;; (lambda (node)
;;   (if (node :cc_library)
;;       (let (attrs (up
;;   )

(define (pprint-node node)
  (display
   (format #f "~NC~A[~A] ~A~A"
           indent
           #\space
           (tokens (node :tid))
           (node :tid)
           (if (node :printable?) "____    " "")
           (if (node :printable?) "(node :pprint)" "")))
  (newline)
  )

(define (test-rule-edits node)
  (display "test-rule-edits")
  (newline)
  ;; (node :rules cc_library)
  ;; refer to rule :foo
  ;; (node :rules :* :attrs :name "foo") ;; ??

  ;; ideally:  (//pkg:tgt :attrs etc.)
  ;; "path" reader macro?  #:(//pkg:tgt ...), #:(pkg ...), etc.
  ;; locally, for file already parsed:
  ;; #:(:tgt ...) resolves :tgt to Call_Expr with Id == tgt
  ;; would have to expand to something in car, not :tgt

  ;; (with-ast my-ast-node
  ;;   #:(:tgt ...)
  ;;    macro obtains ast node from with-ast env.
  ;;    expands to (my-ast-node :targets :tgt ...)
  ;;    where :targets is a psuedo-property of rules node

  ;; #: == target macro; the macro can expand //pkg:tgt
  ;; semantically //pkg:tgt becomes an ast_node
  ;; for rule types. buildozer:  //pkg:%gwt_module
  ;; sunlark:  #/(//pkg%gwt_module ...) ?
  ;; #/ contrast with target macro

  ;; node types - which properties do they understand?

  ;; build-file (root): :package, :loads, :targets
  ;; target-list: ::tgt (root :targets) produces target-list
  ;; target (Call_Expr): :rule, :attrs
  ;; attr-list (Call_Expr/List_Expr): :<attr-name>
  ;; attr:  :name, :value

  ;; all: :pprint

  )

(define (test-attr-updates node)
  (display "................................")
  (newline)

  (if (node :call-expr?) ;;FIXME: :target?
      (begin
        (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>") (newline)
        ;; (let ((strattr (node :@string_attr)))
        ;;   ;; (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>") (newline)
        ;;   ;; (walk-all strattr handlers)
        ;;   ;; (display "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<") (newline)
        ;;   ;; (newline)
        ;;   (set! (strattr 'name) "new_attr")
        ;;   (set! (strattr 'value) "goodbye!")
        ;;   )

        ;; (display (node :rule))
        ;; (newline)

        ;; (if (= ((node :attrs) :tid) (sunlark-tids :arg-list))
        ;;     (display "GOT IT")
        ;;     (display "OOPS"))
        ;; (newline)

        ;; (pprint-node (node :attrs))
        ;; (newline)

        ;; (display (format #f "(node :attrs :deps): ~A"
        ;;                  (node :attrs :deps)))

        (display (node :attrs :deps))

        ;; (ast-node-set! node :attrs :deps :name "newdeps")
        ;; (set! (node :attrs :deps :name) "newdeps")
        ;; (set! (node :attrs :deps) "newdeps")
        (newline)

        (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> x") (newline)

;;         (if (node :attrs :deps)
;;             (let ((deps (node :attrs :deps)))
;;               (begin
;;         ;;         ;; (display (format #f "node tid: ~D ~A"
;;         ;;         ;;                  (deps :tid) (tokens (deps :tid))))
;;         ;;         ;; (newline)
;;         ;;         ;; (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>") (newline)
;;         ;;         ;; (walk-all deps handlers)
;;         ;;         ;; (display "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<") (newline)
;;         ;;         ;; (newline)

;;         ;;         (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 1") (newline)
;;         ;;         (newline)
;;         ;;         deps
;;         ;;         (newline)
;;         ;;         (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> 2") (newline)
;;         ;;         (newline)

;;         ;;         ;; (ast-node-ref node :attrs :deps :line)

;;         ;;         ;; (ast-node-ref node :attrs 'deps 'foo)
;;         ;;         ;; (node :attrs 'deps 'name)
;;         ;;         ;; (ast-node-ref node :attrs 'deps 'bar)

;;         ;;         ;; (ast-node-ref node :attrs 'deps 'value)
;;         ;;         ;; (ast-node-ref deps :attrs)

;;         ;;         ;; (deps :attrs)
;;         ;;         ;; (display (format #f "(deps :attrs): ~A"
;;         ;;         ;;                    ;; (deps :attrs)
;;         ;;         ;;                    (ast-node-ref deps :attrs)
;;         ;;         ;;                    ))
;;         ;;         ;; (newline)

;;         ;;         ;; (set! (deps :attrs) (:remove :trailing-comma))

;;  replace attr-name:
;;                 ;; (set! (deps 'name) "newdeps")

;;                 (set! (node :attrs :deps :name) "newdeps")

;;         ;;         ;; value add. what if value already present?

;;         ;;         ;; (set! (deps 'value) '(:add)) ;; error: # args

;;         ;;         ;; (set! (node :attr 'deps 'value)
;;         ;;         ;;       '(:add //foo //foo:bar @c//d:e))

;;         ;;         ;; (set! (deps 'value) '(:add //foo //foo:bar @c//d:e))

;;                 ;; (set! (node :attrs :deps 'value) '(:add "dep_X")) ;; append

;;                 ;; (set! (deps 'value) '(:add :2 "dep_X")) ;;

;;         ;;         ;; (set! (deps 'value) '(:add :0 "dep_X" "dep_Y", "dep_Z")) ;; insert

;;         ;;         ;; (set! (deps 'value) '(:replace "dep_a" "dep_X"))

;;         ;;         ;; replace by posn:
;;         ;;         ;; (set! (deps 'value) '(:replace :0 "foo"))
;;         ;;         ;; multiple:
;;         ;;         ;; (set! (deps 'value) '(:replace :0 "foo" :2 "bar"))
;;         ;;         ;; all - replace all values by ["foo"]
;;         ;;         ;; (set! (deps 'value) '(:replace :* "foo"))
;;         ;;         ;; each - replace each values by "foo"
;;         ;;         ;; (set! (deps 'value) '(:replace :* "foo"))
;;         ;;         ;; what about replacing with e.g. r"foo", b"bar"
;;         ;;         ;; or triple quotes?

;;         ;;         ;; (set! (deps 'value) '(:remove :1 :4))
;;         ;;         ;; remove trailing commas
;;         ;;         ;; (set! (deps 'value) '(:remove :trailing_comma))
;;         ;;         )))
;; )))
)))

(define (default-handler node indent)

;;  (test-queries node)

  (test-attr-updates node)

  ;;(pprint-node node)

  ;; (display (format #f "(node :stmt-list?): ~A" (node :stmt-list?))) (newline)
  ;; (display (format #f "(NODE :stmt-list?): ~A" (node :stmt-list?))) (newline)

  ;; (display
  ;;  (format #f "~NC~A[~A] ~A"
  ;;          indent
  ;;          #\space
  ;;          (tokens (node :tid))
  ;;          (node :tid)))
  ;; (newline)

  )

  ;; (if (node :printable)
  ;;     )

(define indent 0)

(define (walk-all node handlers)
  (display "walk-all")
  ;; (newline)

  (default-handler node indent) ;; just prints
  (set! indent (+ 2 indent))

  ;; (if (node :comments?)
  ;;     (begin
  ;;       (display "processing comments") (newline)
  ;;       ;; comments is a ast_nodelist, which is a vector
  ;;       ))

  (if (node :subnodes)
      (for-each
       (lambda (subnode)
         ;; (display (format #f " walk subnode type: ~A" (subnode :tid))) (newline)
         (walk-all subnode handlers)
         )
       (node :subnodes)))
  (set! indent (- indent 2))
  )
