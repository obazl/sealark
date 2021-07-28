(set! (*s7* 'heap-size) (* 8 1024000))

(define (cpy1 x y) ; opt_dox
  (do ((i 0 (+ i 1))
       (len (length x)))
      ((= i len) y)
    (int-vector-set! y i (int-vector-ref x i))))

(define (tst-cpy1)
  (let ((x (make-int-vector 1000000 123))
	(y (make-int-vector 1000000 0)))
    (cpy1 x y)
    (unless (equal? y (make-int-vector 1000000 123))
      (format *stderr* "y1: ~S~%" y))))

(tst-cpy1)

(define (cpy2 x y) ; opt_dotimes
  (let ((len (length x)))
    (do ((i 0 (+ i 1)))
	((= i len) y)
      (int-vector-set! y i (int-vector-ref x i)))))

(define (tst-cpy2)
  (let ((x (make-int-vector 1000000 123))
	(y (make-int-vector 1000000 0)))
    (cpy2 x y)
    (unless (equal? y (make-int-vector 1000000 123))
      (format *stderr* "y2: ~S~%" y))))

(tst-cpy2)

;;; --------------------------------------------------------------------------------

(define (cpy3 x y) ; opt_dotimes
  (do ((len (length x))
       (i 0 (+ i 1)))
      ((= i len) y)
    (float-vector-set! y i (float-vector-ref x i))))

(define (tst-cpy3)
  (let ((x (make-float-vector 1000000 123.0))
	(y (make-float-vector 1000000 0.0)))
    (cpy3 x y)
    (unless (equal? y (make-float-vector 1000000 123.0))
      (format *stderr* "y3: ~S~%" y))))

(tst-cpy3)

(define (cpy4 x y) ; opt_dotimes
  (let ((len (length x)))
    (do ((i 0 (+ i 1)))
	((= i len) y)
      (float-vector-set! y i (float-vector-ref x i)))))

(define (tst-cpy4)
  (let ((x (make-float-vector 1000000 123.0))
	(y (make-float-vector 1000000 0.0)))
    (cpy4 x y)
    (unless (equal? y (make-float-vector 1000000 123.0))
      (format *stderr* "y4: ~S~%" y))))

(tst-cpy4)

;;; --------------------------------------------------------------------------------

(define (cpy5 x y)
  (do ((len (length x))
       (i 0 (+ i 1)))
      ((= i len) y)
    (vector-set! y i (vector-ref x i))))

(define (tst-cpy5)
  (let ((x (make-vector 1000000 'a))
	(y (make-vector 1000000 #f)))
    (cpy5 x y)
    (unless (equal? y (make-vector 1000000 'a))
      (format *stderr* "y5: ~S~%" y))))

(tst-cpy5)

(define (cpy6 x y) ; opt_do_copy
  (let ((len (length x)))
    (do ((i 0 (+ i 1)))
	((= i len) y)
      (vector-set! y i (vector-ref x i)))))

(define (tst-cpy6)
  (let ((x (make-vector 1000000 'a))
	(y (make-vector 1000000 #f)))
    (cpy6 x y)
    (unless (equal? y (make-vector 1000000 'a))
      (format *stderr* "y6: ~S~%" y))))

(tst-cpy6)

;;; --------------------------------------------------------------------------------

(define (cpy7 x y)
  (do ((len (length x))
       (i 0 (+ i 1)))
      ((= i len) y)
    (string-set! y i (string-ref x i))))

(define (tst-cpy7)
  (let ((x (make-string 1000000 #\a))
	(y (make-string 1000000 #\b)))
    (cpy7 x y)
    (unless (equal? y (make-string 1000000 #\a))
      (format *stderr* "y7: ~S~%" y))))

(tst-cpy7)

(define (cpy8 x y) ; opt_do_copy
  (let ((len (length x)))
    (do ((i 0 (+ i 1)))
	((= i len) y)
      (string-set! y i (string-ref x i)))))

(define (tst-cpy8)
  (let ((x (make-string 1000000 #\a))
	(y (make-string 1000000 #\b)))
    (cpy8 x y)
    (unless (equal? y (make-string 1000000 #\a))
      (format *stderr* "y8: ~S~%" y))))

(tst-cpy8)

;;; --------------------------------------------------------------------------------

(define (cpy9 x y)
  (do ((len (length x))
       (i 0 (+ i 1)))
      ((= i len) y)
    (list-set! y i (list-ref x i))))

(define (tst-cpy9)
  (let ((x (make-list 10000 #\a))
	(y (make-list 10000 #\b)))
    (cpy9 x y)
    (unless (equal? y (make-list 10000 #\a))
      (format *stderr* "y9: ~S~%" y))))

(tst-cpy9)

(define (cpy10 x y) ; opt_do_copy
  (let ((len (length x)))
    (do ((i 0 (+ i 1)))
	((= i len) y)
      (list-set! y i (list-ref x i)))))

(define (tst-cpy10)
  (let ((x (make-list 10000 #\a))
	(y (make-list 10000 #\b)))
    (cpy10 x y)
    (unless (equal? y (make-list 10000 #\a))
      (format *stderr* "y10: ~S~%" y))))

(tst-cpy10)

;;; --------------------------------------------------------------------------------

(define (cpy11 x y)
  (let loop ((x x) (y y))
    (when (pair? x)
      (set-car! y (car x))
      (loop (cdr x) (cdr y))))
  y)

(define (tst-cpy11)
  (let ((x (make-list 200000 #\a))
	(y (make-list 200000 #\b)))
    (cpy11 x y)
    (unless (equal? y (make-list 200000 #\a))
      (format *stderr* "y11: ~S~%" y))))

(tst-cpy11)

(define (cpy12 x y)
  (let ((len (- (length x) 1)))
    (when (>= len 0)
      (let loop ((i 0))
	(vector-set! y i (vector-ref x i))
	(if (< i len)
	    (loop (+ i 1))))))
  y)

(define (tst-cpy12)
  (let ((x (make-vector 200000 #\a))
	(y (make-vector 200000 #\b)))
    (cpy12 x y)
    (unless (equal? y (make-vector 200000 #\a))
      (format *stderr* "y12: ~S~%" y))))

(tst-cpy12)

;;; --------------------------------------------------------------------------------

(define (f1 v)
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (vector-set! v i 2.0)))

(define (ftest1)
  (let ((v (make-vector 1000)))
    (do ((i 0 (+ i 1)))
	((= i 10000) v)
      (f1 v))))

(ftest1)


(define (f2 v)
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (string-set! v i #\a)))

(define (ftest2)
  (let ((v (make-string 1000)))
    (do ((i 0 (+ i 1)))
	((= i 10000) v)
      (f2 v))))

(ftest2)


(define (f3 v)
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (float-vector-set! v i 2.0)))

(define (ftest3)
  (let ((v (make-float-vector 1000)))
    (do ((i 0 (+ i 1)))
	((= i 10000) v)
      (f3 v))))

(ftest3)


(define (f4 v)
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (int-vector-set! v i 2)))

(define (ftest4)
  (let ((v (make-int-vector 1000)))
    (do ((i 0 (+ i 1)))
	((= i 10000) v)
      (f4 v))))

(ftest4)


(define (f5 v)
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (list-set! v i 2.0)))

(define (ftest5)
  (let ((v (make-list 1000)))
    (do ((i 0 (+ i 1)))
	((= i 1000) v)
      (f5 v))))

(ftest5)


;;; --------------------------------------------------------------------------------

(let ((new-env (sublet (curlet) (cons 'init_func 'block_init)))) ; load calls init_func if possible
  ;; depends on running s7test first normally
  (load "s7test-block.so" new-env))

(define (test-copy size)
  (let ((old-string (make-string size #\a))
	(old-bvect (make-byte-vector size 1))
	(old-pair (make-list size #\a))
	(old-vector (make-vector size #\a char?))
	(old-vectorf (make-vector size 1.0 real?))
	(old-vectori (make-vector size 1 integer?))
	(old-fvect (make-float-vector size 1.0))
	(old-ivect (make-int-vector size 1))
	(old-hash (make-hash-table size))
	(old-let (inlet))
	(old-block (make-block size)))

    (do ((i 0 (+ i 1)))
	((= i size))
      (hash-table-set! old-hash (string->symbol (number->string i)) #\a)) ; gensym is slower, even with "" as prefix 
    (copy old-hash old-let)

    (let ((new-string (make-string size #\space))
	  (new-bvect (make-byte-vector size 0))
	  (new-pair (make-list size 1))
	  (new-vector (make-vector size 1))
	  (new-fvect (make-float-vector size 1.0))
	  (new-ivect (make-int-vector size 1))
	  (new-hash (make-hash-table (* size 2)))
	  (new-let (inlet))
	  (new-block (make-block size)))
      
      (copy old-string)
      (copy old-pair)
      (copy old-vector)
      (copy old-fvect)
      (copy old-ivect)
      (copy old-hash)
      (copy old-let)
      (copy old-bvect)
      (copy old-block)
      
      (length old-string)
      (length old-pair)
      (length old-vector)
      (length old-fvect)
      (length old-ivect)
      (length old-hash)
      (length old-let)
      (length old-bvect)
      (length old-block)
      
      (fill! old-string #\a)
      (fill! new-string #\space)
      (fill! old-pair #\a)
      (fill! new-pair 1)
      (fill! old-vector #\a)
      (fill! new-vector 1)
      (fill! old-fvect 1.0)
      (fill! new-fvect 1.0)
      (fill! old-ivect 1)
      (fill! new-ivect 1)
      (fill! old-bvect 1)
      (fill! new-bvect 0)
      (fill! old-block 0.0)
      (fill! new-block 1.0)
      
      (copy old-string new-string)
      (copy old-vector new-string)
      (copy old-pair new-string)
      (copy old-bvect new-string)
      
      (copy old-bvect new-bvect)
      (copy old-ivect new-bvect)
      (copy old-vectori new-bvect)
      (copy old-string new-bvect)
      
      (copy old-pair new-pair)
      (copy old-string new-pair)
      (copy old-vector new-pair)
      (copy old-hash new-pair)
      (copy old-fvect new-pair)
      (copy old-ivect new-pair)
      (copy old-let new-pair)
      (copy old-block new-pair)
      (set! new-pair #f)
      
      (copy old-vector new-vector)
      (copy old-pair new-vector)
      (copy old-string new-vector)
      (copy old-fvect new-vector)
      (copy old-ivect new-vector)
      (copy old-hash new-vector)
      (copy old-let new-vector)
      (copy old-block new-vector)
      (set! new-vector #f)
      
      (copy old-fvect new-fvect)
      (copy old-ivect new-fvect)
      (copy old-vectorf new-fvect)
      (copy old-block new-fvect)
      
      (copy old-ivect new-ivect)
      (copy old-fvect new-ivect)
      (copy old-vectori new-ivect)
      (copy old-bvect new-ivect)
      
      (copy old-hash new-hash)
      (copy old-let new-hash)
      
      (copy old-let new-let)
      
      (copy old-fvect new-block)
      (copy old-block new-block))
    
    (let ((nsize (/ size 2))
	  (start (/ size 4)))
      (let ((new-string (make-string size #\space))
	    (new-pair (make-list size 1))
	    (new-vector (make-vector size 1))
	    (new-fvect (make-float-vector size 1.0))
	    (new-ivect (make-int-vector size 1))
	    (new-hash (make-hash-table (* size 2)))
	    (new-let (inlet))
	    (new-bvect (make-byte-vector size 255))
	    (new-block (make-block size)))
	
	(copy old-string new-string start (+ start nsize))
	(copy old-vector new-string start (+ start nsize))
	(copy old-pair new-string start (+ start nsize))
	
	(copy old-bvect new-bvect start (+ start nsize))
	(copy old-vectori new-bvect start (+ start nsize))
	(copy old-ivect new-bvect start (+ start nsize))
	(copy old-string new-bvect start (+ start nsize))
	
	(copy old-pair new-pair start (+ start nsize))
	(copy old-string new-pair start (+ start nsize))
	(copy old-vector new-pair start (+ start nsize))
	(copy old-hash new-pair start (+ start nsize))
	(copy old-fvect new-pair start (+ start nsize))
	(copy old-ivect new-pair start (+ start nsize))
	(copy old-let new-pair start (+ start nsize))
	(copy old-block new-pair start (+ start nsize))
	(set! new-pair #f)
	
	(copy old-vector new-vector start (+ start nsize))
	(copy old-pair new-vector start (+ start nsize))
	(copy old-string new-vector start (+ start nsize))
	(copy old-fvect new-vector start (+ start nsize))
	(copy old-ivect new-vector start (+ start nsize))
	(copy old-hash new-vector start (+ start nsize))
	(copy old-let new-vector start (+ start nsize))
	(copy old-block new-vector start (+ start nsize))
	(set! new-vector #f)
	
	(copy old-fvect new-fvect start (+ start nsize))
	(copy old-ivect new-fvect start (+ start nsize))
	(copy old-vectorf new-fvect start (+ start nsize))
	(copy old-block new-fvect start (+ start nsize))
	
	(copy old-ivect new-ivect start (+ start nsize))
	(copy old-fvect new-ivect start (+ start nsize))
	(copy old-vectori new-ivect start (+ start nsize))
	(copy old-bvect new-ivect start (+ start nsize))
	
	(copy old-hash new-hash start (+ start nsize))
	(copy old-let new-hash start (+ start nsize))
	
	(copy old-let new-let start (+ start nsize))
	
	(copy old-fvect new-block start (+ start nsize))
	(copy old-block new-block start (+ start nsize))))
    
    (reverse old-string)
    (reverse old-pair)
    (reverse old-vector)
    (reverse old-fvect)
    (reverse old-ivect)
    (reverse old-hash)
    (reverse old-bvect)
    (reverse old-block)
    
    (reverse! old-string)
    (reverse! old-pair)
    (reverse! old-vector)
    (reverse! old-fvect)
    (reverse! old-ivect)
    (reverse! old-bvect)
    (reverse! old-block)
    ))

(define-expansion (test tst expected)
  `(let ((val ,tst))
     (unless (equal? val ,expected)
       (format *stderr* "~S: ~S but expected ~S~%" ',tst val ,expected))))

(define (test-append size)
  (let ((strs ())
	(vecs ())
	(fvecs ())
	(ivecs ())
	(ifvecs ())
	(allvecs ())
	(bvecs ())
	(lsts ()))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! strs (cons (make-string size (integer->char (+ 1 (random 255)))) strs))
      (set! bvecs (cons (string->byte-vector (make-string size (integer->char (random 256)))) bvecs))
      (set! vecs (cons (make-vector size i) vecs))
      (set! ivecs (cons (make-int-vector size i) ivecs))
      (set! fvecs (cons (make-float-vector size (* i 1.0)) fvecs))
      (set! ifvecs (cons ((if (even? i) make-float-vector make-int-vector) size (if (even? i) (* i 1.0) i)) ifvecs))
      (set! allvecs (cons (make-vector size (if (even? i) (* i 1.0) i)) allvecs))
      (set! lsts (cons (make-list size i) lsts)))
    (let ((lst (apply append lsts))
	  (vec (apply vector-append vecs))
	  (fvec (apply vector-append fvecs))
	  (ivec (apply vector-append ivecs))
	  (ifvec (apply vector-append ifvecs))
	  (allvec (apply vector-append allvecs))
	  (str (apply string-append strs))
	  (bvec (apply vector-append bvecs)))
      (test (vector? vec) #t)
      (test (length vec) (* size size))
      (test (float-vector? fvec) #t)
      (test (length fvec) (* size size))
      (test (int-vector? ivec) #t)
      (test (length ivec) (* size size))
      (test (vector? allvec) #t)
      (test (length allvec) (* size size))
      (test (vector? ifvec) #t)
      (test (length ifvec) (* size size))
;      (test (float-vector? ifvec) #t)
;      (test (int-vector? ifvec) #f)
      (test (pair? lst) #t)
      (test (length lst) (* size size))
      (test (string? str) #t)
      (test (length str) (* size size))
      (test (byte-vector? bvec) #t)
      (test (length bvec) (* size size))
      )))
      
(define (t)
  (do ((i 0 (+ i 1)))
      ((= i 10000))
    (test-copy 100))
  (do ((i 1 (* i 10)))
      ((> i 1000))
    (test-append i)))

(t)

#|
(format *stderr* "copy~%")
(test-copy 1000000)
;100000 : .1
;1000000 : 4.6
;10000000 : 356 = about 120 million objects = about 6Gbytes, mark_pair/gc 
;(format *stderr* "append~%")
;(test-append 10000)
|#

(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)
