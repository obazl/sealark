(display "hello from sunlark/scm/ast_walk.scm")
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

(define (default-handler node indent)
  (display
   (format #f "~NC~A[~A] ~A~A"
           indent
           #\space
           (tokens (node :tid))
           (node :tid)
           (if (node :printable?) "____    " "")
           (if (node :printable?) (node :print) "")))
  (newline))

;; (define (foo)
;;   ;; (display (format #f "(node :stmt-list?): ~A" (node :stmt-list?))) (newline)
;;   ;; (display (format #f "(NODE :stmt-list?): ~A" (node :stmt-list?))) (newline)

;;   ;; (display
;;   ;;  (format #f "~NC~A[~A] ~A"
;;   ;;          indent
;;   ;;          #\space
;;   ;;          (tokens (node :tid))
;;   ;;          (node :tid)))
;;   ;; (newline)

;;   (if (node :call-expr?)
;;       (begin
;;         (let ((strattr (node :@string_attr)))
;;           ;; (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>") (newline)
;;           ;; (walk-all strattr handlers)
;;           ;; (display "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<") (newline)
;;           ;; (newline)
;;           (set! (strattr 'name) "new_attr")
;;           (set! (strattr 'value) "goodbye!")
;;           )
;;         (let ((deps (node :@deps)))
;;           ;; (display ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>") (newline)
;;           ;; (walk-all deps handlers)
;;           ;; (display "<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<") (newline)
;;           ;; (newline)
;;           (set! (deps 'name) "foo")
;;           ;; (set! (deps 'value) '(:add)) ;; error: # args
;;           ;; (set! (deps 'value) '(:add "a" "b" "c")) ;; error # args

;;           ;; (set! (deps 'value) '(:add "dep_X")) ;; append
;;           (set! (deps 'value) '(:add "dep_X" "dep_Y", "dep_Z")) ;; append

;;           ;; (set! (deps 'value) '(:add :2 "dep_X")) ;;
;;           ;; (set! (deps 'value) '(:add :0 "dep_X" "dep_Y", "dep_Z")) ;; insert
;;           )
;;         ))
;;   )

  ;; (if (node :printable)
  ;;     )

(define indent 0)

(define (walk-all node handlers)
  ;; (display "walk-all")
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
(display "goodbye from sunlark/scm/ast_walk.scm")
(newline)
