;(set! (*s7* 'profile) 1)
(set! (*s7* 'heap-size) 1024000)

(define-macro (test expr res)
  `(let ((value ,expr))
     (unless (equivalent? value ,res)
       (format *stderr* "~S, expected: ~S, got: ~S~%" ',expr ,res value))))

(require case.scm)
(define (scase x)
  (case* x
      ((a b) 'a-or-b)
      ((1 2/3 3.0) => (lambda (a) (* a 2)))
      ((#_pi) 1 123)
      (("string1" "string2"))
      ((#<symbol?>) 'symbol!)
      (((+ x #<symbol?>)) 'got-list)
      ((#(1 x 3)) 'got-vector)
      (((+ #<>)) 'empty)
      (((* #<x:symbol?> #<x>)) 'got-label)
      (((#<> #<x:> #<x>)) 'repeated)
      (((#<symbol?> #<symbol?>)) 'two)
      (((#<x:> #<x>)) 'pair)
      ((#(#<x:> #<x>)) 'vector)
      ((#(#<symbol?> #<...> #<number?>)) 'vectsn)
      ((#(#<...> #<number?>)) 'vectstart)
      ((#(#<string?> #<char-whitespace?> #<...>)) 'vectstr)
      (else 'oops)))
  
(define (scase5 x)
  (case* x
    (((#<symbol?> #<...> #<symbol?>)) 'ok)))

(let ((local-func (lambda (key) (eqv? key 1))))
  (define (scase1 x)
    (case* x
      ((2 3 a) 'oops)
      ((#<local-func>) 'yup))))

(define scase2
  (let ((local-func (lambda (key) (eqv? key 1))))
    (lambda (x)
      (case* x
	((2 3 a) 'oops)
	((#<local-func>) 'yup)))))

(define (scase3 x)
  (let ((local-func (lambda (key) (eqv? key 1))))
    (case* x
      ((2 3 a) 'oops)
      ((#<local-func>) 'yup))))

(define (ecase x)
  (case* x
    (((#<symbol?> #<...> #<symbol?>)) 'both-symbol)
    (((#<symbol?> #<...>)) 'car-symbol)
    (((#<...> #<symbol?> #<symbol?>)) 'two-symbols)
    (((#<...> #<symbol?>)) 'end-symbol)
    (else #f)))

(define (scase6 x)
  (case* x
    (((#<x:> #<x> #<...>)) 'ok)
    (else 'oops)))

(define (scase7 x)
  (case* x
    (((#<...> #<x:> #<x>)) 'ok)
    (else 'oops)))

(define (scase8 x)
  (case* x
    (((#<x:> #<...> #<x>)) 'ok)
    (else 'oops)))

(define (scase9 x)
  (case* x
    ((#<y>) 1)
    (else 'oops)))

(define (scase10 x)
  (case* x
    ((#<x>) 1)
    (else 'oops)))

(define (scase11 x)
  (case* x
    ((#<...>) 1)
    (else 'oops)))

(define (scase12 x)
  (case* x
    (((#<y:> #<zzz>)) 1)
    (else 'oops)))

(define (scase13 x)
  (case* x
    ((#<>) 'ok) ; matches anything! as does #<label:>
    (else 'oops)))

(define (scase14 x)
  (case* x
    (((#<y:> #<y:>)) 1)
    (else 'oops)))

(define uniquify
  (let ()
    (define (uniq-1 lst new-lst)
      (case* lst
	((()) 
	 (reverse new-lst))
	(((#<>))
	 (reverse (cons (car lst) new-lst)))
	(((#<x:> #<x> #<...>))
	 (uniq-1 (cdr lst) new-lst))
	(else
	 (uniq-1 (cdr lst) (cons (car lst) new-lst)))))
    (lambda (lst)
      (uniq-1 lst ()))))

(define (palindrome? x) ; x can be a list or a vector
  (case* x
    ((() (#<>) 
      #() #(#<>)) 
     #t)
    (((#<x:> #<middle:...> #<x>) 
      #(#<x:> #<middle:...> #<x>))
     (palindrome? #<middle>))
    (else #f)))

(define (scase15 x)
  (case* x
    (((+ #<x:> #<x>)) (* 2 #<x>))
    (((#<x:> #<y:>)) (list #<y> #<x>))
    (else 'oops)))

(define (scase16 x)
  (case* x
    (((+ (* #<symbol?> 2) 3)) 0)
    (else 1)))

(define (scase17 x)
  (let ((a1 3))
    (case* x
      (((+ #<add1:symbol?> (* #<mul1:number?> 2))) (+ #<mul1> (* #<add1> 2)))
      (else 'oops))))

(let ((a1 3))
  (define (scase18 x)
    (case* x
      (((+ #<add1:symbol?> (* #<mul1:number?> 2))) (quote (+ #<mul1> (* #<add1> 2))))
      (else 'oops))))

(define (case-reverse x) ; maybe the least efficient reverse ever
  (case* x
    (((#<>) ()) x)
    (((#<first:> #<rest:...>))
     (append (case-reverse #<rest>) 
	     (list (quote #<first>))))))

(define (scase19 x)
  (case* x
    (((#<integer?> . #<symbol?>)) 'ok)
    (else #f)))

(define (scase20 x)
  (case* x
    ((#(+ (* #<symbol?> 2) 3)) 0)
    (else 1)))

(define scase21
  (let ((pair2? (lambda (p) 
		  (= (length p) 2))))
    (lambda (x)
      (case* x
        (((+ #<pair2?> 3)) #t)
	(else #f)))))

(define scase22
  (letrec ((symbols? 
	    (lambda (x)
	      (or (null? x)
		  (and (pair? x)
		       (and (symbol? (car x))
			    (symbols? (cdr x))))))))
    (lambda (x)
      (case* x
        ((#<symbols?>) #t)
        (else #f)))))

(define scase23
  (let ((numeric-op? (lambda (x)
		       (let ((func (symbol->value x)))
			 (and (signature func)
			      (memq (car (signature func)) '(number? complex? real? float? rational? integer? byte?)))))))
    (lambda (x)
      (case* x
        (((#<numeric-op?> #<number?>)
	  (#<numeric-op?> #<number?> #<number?>)) #t)
        (else #f)))))

(define (scase24 x)
  (case* x
    (((+ #<rest:...>))
     (+ (apply values #<rest>)))
    (else 'oops)))

(define (scase25 x)
  (case* x
    (((#<symbol?> #<ellip1:...> (+ #<ellip2:...>))) (append #<ellip1> #<ellip2>))
    (else #f)))

(define (scase26 x)
  (case* x
    (((if (not #<test:>) (begin #<body:...>))) (cons 'unless (cons '#<test> #<body>)))
    (((if (not #<test:>) #<body:>)) (cons 'unless (list '#<test> '#<body>)))
    (((if #<test:> (begin #<body:...>))) (cons 'when (cons '#<test> #<body>)))
    (((if #<test:> #<body:>)) (cons 'when (list '#<test> '#<body>)))))


(define (scase27 x)
  (let ((efunc? (lambda (x)
		  (and (pair? x)
		       (number? (car x))))))
    (case* x
      (((#<label,efunc?:...>)) #t)
      (else #f))))

(define (scase29 x)
  (let ((match? ((funclet 'case*) 'case*-match?)))
    (let ((multiplier? (lambda (x)
			 (or (match? x '(* 1 #<integer?>))
			     (match? x '(* 2 #<integer?>))))))
      (case* x
	(((+ #<integer?> #<multiplier?> #<integer?>)) #t)
	(else #f)))))

(define (scase30 x)
  (let ((match? ((funclet 'case*) 'case*-match?)))
    (match? x '(+ #<symbol?> 1))))

(define* (scase31 x (e (curlet)))
  (let ((match? ((funclet 'case*) 'case*-match?))
        (labels ((funclet 'case*) 'case*-labels)))
    (and (match? x '(#<symbol?> #<ellip1:...> (+ #<ellip2:...>)))
         (append (cadr (labels 'ellip1)) (cadr (labels 'ellip2))))))

(define (scase32 x)
  (let ((match? ((funclet 'case*) 'case*-match?))
        (labels ((funclet 'case*) 'case*-labels)))
    (if (match? x '(if #<test:> (begin #<body:...>)))
	(cons 'when (cons (labels 'test) (cadr (labels 'body)))))))

(define (scase33 x)
  (case* x
    ((#<"a.b">) #t)
    (else #f)))

(define (scase34 x)
  (case* x
    ((#<reg:"a.b">) #<reg>)
    (else #f)))

(define (scase35 x)
  (let ((quotes? (lambda (x)
		   (char-position #\" x))))
    (case* x
      ((#<"^dog">) 'dog0)
      ((#<"gray\|grey">) 'graey) ; basic regex so it needs \, apparently doesn't work in OSX?
      ((#<"h\(a\|e\)y">) 'haey) 
      ((#<"p[ae]y">) 'paey)
      ((#<"b[aeiou]bble">) 'bxbble)
      ((#<"z\{3,6\}">) 'zzz)
      ((#<"\d">) 'digit)
      ((#<"<>">) 'brackets)
      ((#<quotes?>) 'quotes)
      ((#<"[^i*&2@]">) 'not-i)
      (else #f))))


(test (scase 3.0) 6.0)
(test (scase pi) 123)
(test (scase "string1") "string1")
(test (scase "string3") 'oops)
(test (scase 'a) 'a-or-b)
(test (scase 'abc) 'symbol!)
(test (scase #()) 'oops)
(test (scase '(+ x z)) 'got-list)
(test (scase #(1 x 3)) 'got-vector)
(test (scase '(+ x 3)) 'oops)
(test (scase '(+ x)) 'empty)
(test (scase '(* z z)) 'got-label)
(test (scase '(* z x)) 'oops)
(test (scase '(+ (abs x) (abs x))) 'repeated)
(test (scase '(+ (abs x) (abs y))) 'oops)
(test (scase '(a b)) 'two)
(test (scase '(1 1)) 'pair)
(test (scase '(1 1 2)) 'oops)
(test (scase #(1 1)) 'vector)
(test (scase #(a b c 3)) 'vectsn)
(test (scase #(1 b 2)) 'vectstart)
(test (scase #("asdf" #\space +nan.0 #<eof>)) 'vectstr)
(test (scase #(a 3)) 'vectsn)
(test (scase #(1)) 'vectstart)
(test (scase #("asdf" #\space)) 'vectstr)
(test (scase #("asdf")) 'oops)

(test (scase5 '(a)) #<unspecified>)

(test (scase2 2) 'oops)
(test (scase2 32) #<unspecified>)
(test (scase2 1) 'yup)
(test (scase3 2) 'oops)
(test (scase3 32) #<unspecified>)
(test (scase3 1) 'yup)

(test (scase6 '(a a)) 'ok)
(test (scase7 '(a a)) 'ok)
(test (scase8 '(a a)) 'ok)
(test (catch #t (lambda () (scase9 1)) (lambda (type info) type)) 'oops)
(test (catch #t (lambda () (scase10 1)) (lambda (type info) type)) 'oops)
(test (catch #t (lambda () (scase11 1)) (lambda (type info) type)) 'oops)
(test (catch #t (lambda () (scase12 '(1 2))) (lambda (type info) type)) 'unbound-variable)

(test (ecase '(a b 1)) 'car-symbol)
(test (ecase '(1 2 c)) 'end-symbol)
(test (ecase '(a 1 2 3 c)) 'both-symbol)
(test (ecase '(1 2 3 b c)) 'two-symbols)
(test (scase13 '(a a)) 'ok)
(test (scase13 1+i) 'ok)
(test (scase13 #(1 2 3)) 'ok)
(test (catch #t (lambda () (scase14 '(1 1))) (lambda (type info) type)) 'syntax-error) ; duplicate identifier currently uses this error type
(test (uniquify '(a a b b b b a a c c)) '(a b a c))
(test (uniquify '((+ a 1) (+ a 1) (* b 2) (* b 2) c a a)) '((+ a 1) (* b 2) c a))
(test (uniquify '(a b b c)) '(a b c))
(test (uniquify '(a)) '(a))
(test (uniquify ()) ())

(let ((x '(+ 2 3)))
  (test (case* x
	  (((+ #<> #<>)) (apply + (cdr x)))
	  (else (error 'out-of-range "unimplemented")))
	5))

(test (palindrome? '(a b a)) #t)
(test (palindrome? '(a b c a)) #f)
(test (palindrome? '(a b c b a)) #t)
(test (palindrome? '(a)) #t)
(test (palindrome? ()) #t)

(test (palindrome? #(a b a)) #t)
(test (palindrome? #(a b c a)) #f)
(test (palindrome? #(a b c b a)) #t)
(test (palindrome? #(a)) #t)
(test (palindrome? #()) #t)

(test (case* '(a b a) (((#<start:...> #<symbol?>)) #<start>)) '(a b))
(test (case* '(a) (((#<start:...> #<symbol?>)) #<start>)) ())
(test (case* '(a b) (((#<start:...> #<symbol?>)) #<start>)) '(a))
(test (case* '(a b a) (((#<symbol?> #<end:...>)) #<end>)) '(b a))
(test (case* '(a) (((#<symbol?> #<end:...>)) #<end>)) ())
(test (case* '(a b) (((#<symbol?> #<end:...>)) #<end>)) '(b))
(test (case* '(a b a) (((#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) '(b))
(test (case* '(a a) (((#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) ())
(test (case* '(a b c a) (((#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) '(b c))

(test (case* #(a b a) ((#(#<start:...> #<symbol?>)) #<start>)) '(a b))
(test (case* #(a) ((#(#<start:...> #<symbol?>)) #<start>)) ())
(test (case* #(a b) ((#(#<start:...> #<symbol?>)) #<start>)) '(a))
(test (case* #(a b a) ((#(#<symbol?> #<end:...>)) #<end>)) '(b a))
(test (case* #(a) ((#(#<symbol?> #<end:...>)) #<end>)) ())
(test (case* #(a b) ((#(#<symbol?> #<end:...>)) #<end>)) '(b))
(test (case* #(a b a) ((#(#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) '(b))
(test (case* #(a a) ((#(#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) ())
(test (case* #(a b c a) ((#(#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) '(b c))

(test (scase15 '(1 2)) '(2 1))
(test (scase15 '(+ 1 1)) 2)
(test (scase15 '(+ (* 2 3) (* 2 3))) 12)
(test (scase16 '(+ (* y 2) 3)) 0)
(test (scase16 '(+ (* y 1) 3)) 1)

(test (scase17 '(+ a1 (* 5 2))) 11)

(test (case-reverse '(a b c)) '(c b a))
(test (case-reverse '(a b)) '(b a))
(test (scase19 (cons 1 'a)) 'ok)
(test (scase19 (list 1 'a)) #f)
(test (scase20 #(+ (* y 2) 3)) 0)
(test (scase20 #(+ (* y 1) 3)) 1)
(test (scase21 '(+ (abs x) 3)) #t)
(test (scase21 '(+ (* 2 x) 3)) #f)
(test (scase22 '(+ a b c)) #t)
(test (scase22 '(+ a b 3)) #f)
(test (scase23 '(+ 1 2)) #t)
(test (scase23 '(floor 32.1)) #t)
(test (scase23 '(abs)) #f)
(test (scase24 '(+ 1 2 3)) 6)

(test (scase25 '(a b c d (+ 1 2))) '(b c d 1 2))
(test (scase26 '(if (not (> i 3)) (display i)))                   '(unless (> i 3) (display i)))
(test (scase26 '(if (not (> i 3)) (begin (display i) (newline)))) '(unless (> i 3) (display i) (newline)))
(test (scase26 '(if (> i 3) (display i)))                         '(when (> i 3) (display i)))
(test (scase26 '(if (> i 3) (begin (display i) (newline))))       '(when (> i 3) (display i) (newline)))

(test (scase27 '(1 2 3)) #t)
(test (scase27 '(a 2 3)) #f)
(test (scase27 '(3)) #t)
(test (scase27 ()) #f)
(test (scase29 '(+ 1 (* 1 2) 3)) #t)
(test (scase29 '(+ 1 (* 3 2) 3)) #f)
(test (scase30 '(+ a 1)) #t)
(test (scase30 '(+ 1 1)) #f)
(test (scase31 '(a b c d (+ 1 2))) '(b c d 1 2))
(test (scase32 '(if (> i 3) (begin (display i) (newline)))) '(when (> i 3) (display i) (newline)))
(test (scase32 '(if 32/15 (begin (display i) (newline)))) '(when 32/15 (display i) (newline)))
(test (scase33 "a1b") #t)
(test (scase33 "abc") #f)
(test (scase33 "a123b") #f)
(test (scase33 'a1b) #f)
(test (scase34 "a1b") "a1b")
(test (scase35 "dog") 'dog0)
(test (scase35 "i7+") 'not-i)
(test (scase35 "gray") 'graey)
(test (scase35 "hay") 'haey)
(test (scase35 "pay") 'paey)
(test (scase35 "bubble") 'bxbble)
(test (scase35 "ab0d") 'digit)
(test (scase35 "+-<>-+") 'brackets)
(test (scase35 "zzzz") 'zzz)
(test (scase35 (string #\a #\")) 'quotes)


(define (tst)
  (do ((i 0 (+ i 1)))
      ((= i 1000))
    (scase 3.0) 
    (scase pi) 
    (scase "string1") 
    (scase "string3") 
    (scase 'a) 
    (scase 'abc) 
    (scase #())
    (scase '(+ x z))
    (scase #(1 x 3))
    (scase '(+ x 3))
    (scase '(+ x))
    (scase '(* z z))
    (scase '(* z x))
    (scase '(+ (abs x)))
    (scase '(+ (abs x)))
    (scase '(a b))
    (scase '(1 1))
    (scase '(1 1 2))
    (scase #(1 1))
    (scase #(a b c 3))
    (scase #(1 b 2))
    (scase #("asdf" #\space +nan.0 #<eof>))
    (scase #(a 3))
    (scase #(1))
    (scase #("asdf" #\space))
    (scase #("asdf"))
    
    (scase5 '(a))
    
    (scase2 2) 
    (scase2 32) 
    (scase2 1) 
    (scase3 2) 
    (scase3 32) 
    (scase3 1) 
    
    (scase6 '(a a))
    (scase7 '(a a))
    (scase8 '(a a))
    (catch #t (lambda () (scase9 1)) (lambda (type info) type))
    (catch #t (lambda () (scase10 1)) (lambda (type info) type))
    (catch #t (lambda () (scase11 1)) (lambda (type info) type))
    (catch #t (lambda () (scase12 '(1 2))) (lambda (type info) type))
    
    (ecase '(a b 1))
    (ecase '(1 2 c))
    (ecase '(a 1 2 3 c))
    (ecase '(1 2 3 b c))
    (scase13 '(a a))
    (scase13 1+i)
    (scase13 #(1 2 3))
    (catch #t (lambda () (scase14 '(1 1))) (lambda (type info) type))
    
    (uniquify '(a a b b b b a a c c))
    (uniquify '((+ a 1) (+ a 1) (* b 2) (* b 2) c a a))
    (uniquify '(a b b c))
    (uniquify '(a))
    (uniquify ())
    
    (let ((x '(+ 2 3)))
      (case* x
	(((+ #<> #<>)) (apply + (cdr x)))
	(else (error 'out-of-range "unimplemented"))))
    
    (palindrome? '(a b a)) 
    (palindrome? '(a b c a))
    (palindrome? '(a b c b a))
    (palindrome? '(a))
    (palindrome? ()) 
    
    (palindrome? #(a b a)) 
    (palindrome? #(a b c a)) 
    (palindrome? #(a b c b a))
    (palindrome? #(a)) 
    (palindrome? #())
    
    (case* '(a b a) (((#<start:...> #<symbol?>)) #<start>))
    (case* '(a) (((#<start:...> #<symbol?>)) #<start>)) 
    (case* '(a b) (((#<start:...> #<symbol?>)) #<start>))
    (case* '(a b a) (((#<symbol?> #<end:...>)) #<end>))
    (case* '(a) (((#<symbol?> #<end:...>)) #<end>))
    (case* '(a b) (((#<symbol?> #<end:...>)) #<end>))
    (case* '(a b a) (((#<symbol?> #<middle:...> #<symbol?>)) #<middle>))
    (case* '(a a) (((#<symbol?> #<middle:...> #<symbol?>)) #<middle>))
    (case* '(a b c a) (((#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) 
    
    (case* #(a b a) ((#(#<start:...> #<symbol?>)) #<start>))
    (case* #(a) ((#(#<start:...> #<symbol?>)) #<start>))
    (case* #(a b) ((#(#<start:...> #<symbol?>)) #<start>)) 
    (case* #(a b a) ((#(#<symbol?> #<end:...>)) #<end>)) 
    (case* #(a) ((#(#<symbol?> #<end:...>)) #<end>)) 
    (case* #(a b) ((#(#<symbol?> #<end:...>)) #<end>)) 
    (case* #(a b a) ((#(#<symbol?> #<middle:...> #<symbol?>)) #<middle>)) 
    (case* #(a a) ((#(#<symbol?> #<middle:...> #<symbol?>)) #<middle>))
    (case* #(a b c a) ((#(#<symbol?> #<middle:...> #<symbol?>)) #<middle>))
    
    (scase15 '(1 2))
    (scase15 '(+ 1 1))
    (scase15 '(+ (* 2 3) (* 2 3))) 
    (scase16 '(+ (* y 2) 3))
    (scase16 '(+ (* y 1) 3))
    
    (scase17 '(+ a1 (* 5 2)))
    
    (case-reverse '(a b c))
    (case-reverse '(a b)) 
    (scase19 (cons 1 'a)) 
    (scase19 (list 1 'a)) 
    (scase20 #(+ (* y 2) 3))
    (scase20 #(+ (* y 1) 3))
    (scase21 '(+ (abs x) 3))
    (scase21 '(+ (* 2 x) 3))
    (scase22 '(+ a b c))
    (scase22 '(+ a b 3))
    (scase23 '(+ 1 2)) 
    (scase23 '(floor 32.1)) 
    (scase23 '(abs)) 
    (scase24 '(+ 1 2 3))
    
    (scase25 '(a b c d (+ 1 2)))
    (scase26 '(if (not (> i 3)) (display i)))
    (scase26 '(if (not (> i 3)) (begin (display i) (newline))))
    (scase26 '(if (> i 3) (display i)))
    (scase26 '(if (> i 3) (begin (display i) (newline))))
    
    (scase27 '(1 2 3))
    (scase27 '(a 2 3))
    (scase27 '(3))
    (scase27 ())
    (scase29 '(+ 1 (* 1 2) 3))
    (scase29 '(+ 1 (* 3 2) 3))
    (scase30 '(+ a 1))
    (scase30 '(+ 1 1))
    (scase31 '(a b c d (+ 1 2)))
    (scase32 '(if (> i 3) (begin (display i) (newline))))
    (scase32 '(if 32/15 (begin (display i) (newline))))
    (scase33 "a1b")
    (scase33 "abc")
    (scase33 "a123b")
    (scase33 'a1b)
    (scase34 "a1b")
    (scase35 "dog")
    (scase35 "i7+")
    (scase35 "gray")
    (scase35 "hay")
    (scase35 "pay")
    (scase35 "bubble")
    (scase35 "ab0d")
    (scase35 "+-<>-+")
    (scase35 "zzzz")
    (scase35 (string #\a #\"))
    ))

(tst)


; (when (> (*s7* 'profile) 0) (show-profile 500))


(exit)
