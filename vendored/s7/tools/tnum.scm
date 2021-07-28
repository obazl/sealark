;;; a first stab at a numerics timing test
(set! (*s7* 'heap-size) (* 6 1024000))

(define dolph-1
  (let ((+documentation+ "(dolph-1 n gamma) produces a Dolph-Chebyshev FFT data window of 'n' points using 'gamma' as the window parameter."))
    (lambda (N gamma)
      (let ((vals (make-vector N)))
	(let ((alpha (cosh (/ (acosh (expt 10.0 gamma)) N))))
	  (do ((den (/ 1.0 (cosh (* N (acosh alpha)))))
	       (freq (/ pi N))
	       (mult -1 (- mult))
	       (i 0 (+ i 1))
	       (phase (* -0.5 pi)))
	      ((= i N))
	    (set! (vals i) (* mult den (cos (* N (acos (* alpha (cos phase)))))))
	    (set! phase (+ phase freq))))
	;; now take the DFT
	(let ((pk 0.0)
	      (w (make-vector N))
	      (c (/ (* 2.0 0+1.0i pi) N)))
	  (do ((i 0 (+ i 1))
	       (sum 0.0 0.0))
	      ((= i N))
	    (do ((k 0 (+ k 1))
		 (cin (* c i)))
		((= k N))
	      (set! sum (+ sum (* (vals k) (exp (* cin k))))))
	    (set! pk (max pk (vector-set! w i (magnitude sum)))))
	  ;; scale to 1.0 (it's usually pretty close already, that is pk is close to 1.0)
	  (do ((i 0 (+ i 1)))
	      ((= i N))
	    (vector-set! w i (/ (vector-ref w i) pk)))
	  w)))))

(dolph-1 (expt 2 10) 0.5)


(define src-duration
  (let ((+documentation+ "(src-duration envelope) returns the new duration of a sound after using 'envelope' for time-varying sampling-rate conversion"))
    (lambda (e)
      (let ((len (- (length e) 2)))
	(do ((all-x (- (e len) (e 0))) ; last x - first x
	     (dur 0.0)
	     (i 0 (+ i 2)))
	    ((>= i len) dur)
	  (let ((area (let ((x0 (e i))
			    (x1 (e (+ i 2)))
			    (y0 (e (+ i 1))) ; 1/x x points
			    (y1 (e (+ i 3))))
			(if (< (abs (real-part (- y0 y1))) .0001)
			    (/ (- x1 x0) (* y0 all-x))
			    (/ (* (log (/ y1 y0)) 
				  (- x1 x0)) 
			       (* (- y1 y0) all-x))))))
	    (set! dur (+ dur (abs area)))))))))


(define (src-test)
  (src-duration (do ((env (make-float-vector 7000))
		     (i 0 (+ i 2)))
		    ((= i 7000) env)
		  (set! (env i) i)
		  (set! (env (+ i 1)) (+ .1 (random 1.0))))))
(src-test)


(define invert-matrix
  (let ((+documentation+ "(invert-matrix matrix b (zero 1.0e-7)) inverts 'matrix'"))
    (lambda* (matrix b (zero 1.0e-7))
      ;; translated from Numerical Recipes (gaussj)
      (call-with-exit
       (lambda (return)
	 (let ((n (car (vector-dimensions matrix))))
	   (let ((cols (make-int-vector n 0))
		 (rows (make-int-vector n 0))
		 (pivots (make-int-vector n 0)))
	     (do ((i 0 (+ i 1))
		  (col 0 0)
		  (row 0 0))
		 ((= i n))
	       (do ((biggest 0.0)
		    (j 0 (+ j 1)))
		   ((= j n)
		    (if (< biggest zero) 
			(return #f))) ; this can be fooled (floats...): (invert-matrix (subvector (float-vector 1 2 3 3 2 1 4 5 6) 0 9 (list 3 3)))
		 (if (not (= (pivots j) 1))
		     (do ((k 0 (+ k 1)))
			 ((= k n))
		       (if (= (pivots k) 0)
			   (let ((val (abs (matrix j k))))
			     (when (> val biggest)
			       (set! col k)
			       (set! row j)
			       (set! biggest val)))
			   (if (> (pivots k) 1)
			       (return #f))))))
	       (set! (pivots col) (+ (pivots col) 1))
	       (if (not (= row col))
		   (let ((temp (if (sequence? b) (b row) 0.0)))
		     (when (sequence? b)
		       (set! (b row) (b col))
		       (set! (b col) temp))
		     (do ((k 0 (+ k 1)))
			 ((= k n))
		       (set! temp (matrix row k))
		       (set! (matrix row k) (matrix col k))
		       (set! (matrix col k) temp))))
	       (set! (cols i) col)
	       (set! (rows i) row)
	       ;; round-off troubles here
	       (if (< (abs (matrix col col)) zero)
		   (return #f))
	       (let ((inverse-pivot (/ 1.0 (matrix col col))))
		 (set! (matrix col col) 1.0)
		 (do ((k 0 (+ k 1)))
		     ((= k n))
		   (set! (matrix col k) (* inverse-pivot (matrix col k))))
		 (if b (set! (b col) (* inverse-pivot (b col)))))
	       (do ((k 0 (+ k 1)))
		   ((= k n))
		 (if (not (= k col))
		     (let ((scl (matrix k col)))
		       (set! (matrix k col) 0.0)
		       (do ((m 0 (+ 1 m)))
			   ((= m n))
			 (set! (matrix k m) (- (matrix k m) (* scl (matrix col m)))))
		       (if b (set! (b k) (- (b k) (* scl (b col)))))))))
	     (do ((i (- n 1) (- i 1)))
		 ((< i 0))
	       (if (not (= (rows i) (cols i)))
		   (do ((k 0 (+ k 1)))
		       ((= k n))
		     (let ((temp (matrix k (rows i))))
		       (set! (matrix k (rows i)) (matrix k (cols i)))
		       (set! (matrix k (cols i)) temp)))))
	     (list matrix b))))))))

(define (matrix-solve A b)
  (cond ((invert-matrix A b) => cadr) (else #f)))

(define (invert-test)
  (matrix-solve (do ((A (make-float-vector '(100 100)))
		     (i 0 (+ i 1)))
		    ((= i 100) A)
		  (do ((j 0 (+ j 1)))
		      ((= j 100))
		    (set! (A i j) (random 1.0))))
		(do ((b (make-float-vector 100))
		     (i 0 (+ i 1)))
		    ((= i 100) b)
		  (set! (b i) (random 1.0)))))
(invert-test)


(define* (cfft data n (dir 1)) ; complex data
  (unless n (set! n (length data)))
  (do ((i 0 (+ i 1))
       (j 0))
      ((= i n))
    (if (> j i)
	(let ((temp (data j)))
	  (set! (data j) (data i))
	  (set! (data i) temp)))
    (do ((m (/ n 2) (/ m 2)))
        ((not (<= 2 m j))
         (set! j (+ j m)))
     (set! j (- j m))))
  (do ((ipow (floor (log n 2)))
       (prev 1)
       (lg 0 (+ lg 1))
       (mmax 2 (* mmax 2))
       (pow (/ n 2) (/ pow 2))
       (theta (complex 0.0 (* pi dir)) (* theta 0.5)))
      ((= lg ipow))
    (do ((wpc (exp theta))
         (wc 1.0)
         (ii 0 (+ ii 1)))
	((= ii prev)
	 (set! prev mmax))
      (do ((jj 0 (+ jj 1))
           (i ii (+ i mmax))
           (j (+ ii prev) (+ j mmax))
	   (tc 0.0))
          ((>= jj pow)
	   (set! wc (* wc wpc)))
        (set! tc (* wc (data j)))
	(set! (data j) (- (data i) tc))
	(set! (data i) (+ (data i) tc)))))
  data)

(define (cfft-test)
  (let* ((size (expt 2 16))
	 (data (make-vector size)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (set! (data i) (complex (- 1.0 (random 2.0)) (- 1.0 (random 2.0)))))
    (cfft data size)))

(cfft-test)


;; these Bessel functions are from Numerical Recipes
(define (bes-j0-1 x)				;returns J0(x) for any real x
  (if (< (abs x) 8.0)
      (let ((y (* x x)))
	(let ((ans1 (+ 57568490574.0000 (* y (- (* y (+ 651619640.7 (* y (- (* y (+ 77392.33017 (* y -184.9052456))) 11214424.18)))) 13362590354.0))))
	      (ans2 (+ 57568490411.0 
		       (* y (+ 1029532985.0 
			       (* y (+ 9494680.718
				       (* y (+ 59272.64853
					       (* y (+ 267.8532712 y)))))))))))
	  (/ ans1 ans2)))
      (let* ((ax (abs x))
	     (z (/ 8.0 ax))
	     (y (* z z)))
	(let ((xx (- ax 0.785398164))
	      (ans1 (+ 1.0 (* y (- (* y (+ 2.734510407e-05 (* y (- (* y 2.093887211e-07) 2.073370639e-06)))) 0.001098628627))))
	      (ans2 (- (* y (+ 0.0001 (* y (- (* y (+ 7.621095160999999e-07 (* y -9.34945152e-08))) 6.911147651000001e-06)))) 0.0156)))
	  (* (sqrt (/ 0.636619772 ax))
	     (- (* ans1 (cos xx))
		(* z (sin xx) ans2)))))))

(define bes-j1-1 
  (let ((signum (lambda (x) (if (= x 0.0) 0 (if (< x 0.0) -1 1)))))
    (lambda (x)				;returns J1(x) for any real x
      (if (< (abs x) 8.0)
	  (let ((y (* x x)))
	    (let ((ans1 (* x (+ 72362614232.0000 (* y (- (* y (+ 242396853.1 (* y (- (* y (+ 15704.4826 (* y -30.16036606))) 2972611.439)))) 7895059235.0)))))
		  (ans2 (+ 144725228442.0 
			   (* y (+ 2300535178.0 
				   (* y (+ 18583304.74
					   (* y (+ 99447.43394
						   (* y (+ 376.9991397 y)))))))))))
	      (/ ans1 ans2)))
	  (let* ((ax (abs x))
		 (z (/ 8.0 ax))
		 (y (* z z)))
	    (let ((xx (- ax 2.356194491))
		  (ans1 (+ 1.0 (* y (+ 0.00183105 (* y (- (* y (+ 2.457520174e-06 (* y -2.40337019e-07))) 3.516396496e-05))))))
		  (ans2 (+ 0.0469 (* y (- (* y (+ 8.449199096000001e-06 (* y (- (* y 1.05787412e-07) 8.8228987e-07)))) 0.0002002690873)))))
	      (* (signum x)
		 (sqrt (/ 0.636619772 ax))
		 (- (* ans1 (cos xx))
		    (* z (sin xx) ans2)))))))))

(define (bes-jn nn x)
  (let ((besn (let ((n (abs nn)))
		(cond ((= n 0) (bes-j0-1 x))
		      ((= n 1) (bes-j1-1 x))
		      ((= x 0.0) 0.0)
		      (else
		       (let ((iacc 40)
			     (ans 0.0000)
			     (bigno 1.0e10)
			     (bigni 1.0e-10))
			 (if (> (abs x) n)
			     (do ((tox (/ 2.0 (abs x)))
				  (bjm (bes-j0-1 (abs x)))
				  (bj (bes-j1-1 (abs x)))
				  (j 1 (+ j 1))
				  (bjp 0.0))
				 ((= j n) (set! ans bj))
			       (set! bjp (- (* j tox bj) bjm))
			       (set! bjm bj)
			       (set! bj bjp))
			     (let ((tox (/ 2.0 (abs x)))
				   (m (* 2 (floor (/ (+ n (sqrt (* iacc n))) 2))))
				   (jsum 0)
				   (bjm 0.0000)
				   (sum 0.0000)
				   (bjp 0.0000)
				   (bj 1.0000))
			       (do ((j m (- j 1)))
				   ((= j 0))
				 (set! bjm (- (* j tox bj) bjp))
				 (set! bjp bj)
				 (set! bj bjm)
				 (when (> (abs bj) bigno)
				   (set! bj (* bj bigni))
				   (set! bjp (* bjp bigni))
				   (set! ans (* ans bigni))
				   (set! sum (* sum bigni)))
				 (if (not (= 0 jsum))
				     (set! sum (+ sum bj)))
				 (set! jsum (- 1 jsum))
				 (if (= j n) (set! ans bjp)))
			       (set! ans (/ ans (- (* 2.0 sum) bj)))))
			 (if (and (< x 0.0) (odd? n))
			     (- ans)
			     ans)))))))
    (if (and (< nn 0)
	     (odd? nn))
	(- besn)
	besn)))

(define (bes-i0 x)
  (if (< (abs x) 3.75)
      (let ((y (expt (/ x 3.75) 2)))
	(+ 1.0
	   (* y (+ 3.5156229
		   (* y (+ 3.0899424
			   (* y (+ 1.2067492
				   (* y (+ 0.2659732
					   (* y (+ 0.360768e-1
						   (* y 0.45813e-2)))))))))))))
      (let* ((ax (abs x))
	     (y (/ 3.75 ax)))
	(* (/ (exp ax) (sqrt ax))
	   (+ 0.3989 (* y (+ 0.0133 
			     (* y (+ 0.0023 
				     (* y (- (* y (+ 0.0092 
						     (* y (- (* y (+ 0.02635537 
								     (* y (- (* y 0.00392377) 0.01647633)))) 
							     0.02057706)))) 
					     0.0016)))))))))))

(define (bes-i1 x)				;I1(x)
  (if (< (abs x) 3.75)
      (let ((y (expt (/ x 3.75) 2)))
	(* x (+ 0.5
		(* y (+ 0.87890594
			(* y (+ 0.51498869
				(* y (+ 0.15084934
					(* y (+ 0.2658733e-1
						(* y (+ 0.301532e-2
							(* y 0.32411e-3))))))))))))))
      (let ((ax (abs x)))
	(let ((ans2 (let* ((y (/ 3.75 ax))
			   (ans1 (+ 0.02282967 (* y (- (* y (+ 0.01787654 (* y -0.00420059))) 0.02895312)))))
		      (+ 0.39894228 (* y (- (* y (- (* y (+ 0.00163801 (* y (- (* y ans1) 0.01031555)))) 0.00362018)) 0.03988024)))))
	      (sign (if (< x 0.0) -1.0 1.0)))
	  (/ (* (exp ax) ans2 sign) (sqrt ax))))))

(define (bes-in n x)
  (cond ((= n 0) (bes-i0 x))
	((= n 1) (bes-i1 x))
	((= x 0.0) 0.0)
	(else
	 (let ((bigno 1.0e10)
	       (bigni 1.0e-10)
	       (ans 0.0000)
	       (tox (/ 2.0 (abs x)))
	       (bip 0.0000)
	       (bi 1.0000)
	       (m (* 2 (+ n (truncate (sqrt (* 40 n)))))) ; iacc=40
	       (bim 0.0000))
	   (do ((j m (- j 1)))
	       ((= j 0))
	     (set! bim (+ bip (* j tox bi)))
	     (set! bip bi)
	     (set! bi bim)
	     (when (> (abs bi) bigno)
	       (set! ans (* ans bigni))
	       (set! bi (* bi bigni))
	       (set! bip (* bip bigni)))
	     (if (= j n) (set! ans bip)))
	   (if (and (< x 0.0) (odd? n))
	       (set! ans (- ans)))
	   (* ans (/ (bes-i0 x) bi))))))


(define (fm-complex-component freq-we-want wc wm a b interp using-sine)
  (let ((sum 0.0)
	(mxa (ceiling (* 7 a)))
	(mxb (ceiling (* 7 b))))
    (do ((k (- mxa) (+ k 1)))
	((>= k mxa))
      (do ((bes-ja (bes-jn k a))
	   (freqwc (- freq-we-want wc (* k wm)))
	   (j (- mxb) (+ j 1)))
	  ((>= j mxb))
	(if (< (abs (- freqwc (* j wm))) 0.1)
	    (set! sum (+ sum (* bes-ja
				(bes-in (abs j) b)
				(expt 0.0+1.0i j)))))))
    (list sum
	  (+ (* (- 1.0 interp) (real-part sum))
	     (* interp (imag-part sum))))))

(define (fm-cascade-component freq-we-want wc wm1 a wm2 b)
  (let ((sum 0.0)
	(mxa (ceiling (* 7 a)))
	(mxb (ceiling (* 7 b))))
    (do ((k (- mxa) (+ k 1)))
	((>= k mxa))
      (do ((bes-ja (bes-jn k a))
	   (freqwc (- freq-we-want wc (* k wm1)))
	   (j (- mxb) (+ j 1)))
	  ((>= j mxb))
	(if (< (abs (- freqwc (* j wm2))) 0.1)
	    (set! sum (+ sum (* bes-ja (bes-jn j (* k b))))))))
    sum))

(define (test-fm-components)
  (do ((i 0.0 (+ i .1)))
      ((>= i 3.0))
    (do ((k 1.0 (+ k 1.0)))
	((>= k 10.0))
      (fm-complex-component 1200 1000 100 i k 0.0 #f)
      (fm-cascade-component 1200 1000 100 i 50 k))))

(test-fm-components)


(define (pisum)             ; from Julia microbenchmarks
  (let ((sum 0.0))
    (do ((j 0 (+ j 1)))
	((= j 500)
	 sum)
      (set! sum 0.0)
      (do ((k 1 (+ k 1)))
	  ((> k 10000))
	(set! sum (+ sum (/ (* k k))))))))

(define (quicksort a lo hi) ; from Julia microbenchmarks (slightly optimized for s7)
  (do ((i lo)
       (j hi hi))
      ((>= i hi))
    (do ((t (a 0))
	 (pivot (a (floor (/ (+ lo hi) 2)))))
	((> i j))
      (set! i (do ((k i (+ k 1)))
		  ((>= (a k) pivot) k)))
      (set! j (do ((k j (- k 1)))
		  ((<= (a k) pivot) k)))
      (when (<= i j)
	(set! t (a i))
	(set! (a i) (a j))
	(set! (a j) t)
	(set! i (+ i 1))
	(set! j (- j 1))))
    (if (< lo j)
	(quicksort a lo j))
    (set! lo i)))

(define (jtests)
  (let-temporarily (((*s7* 'equivalent-float-epsilon) 1e-12))
    (unless (equivalent? (pisum) 1.6448340718480652)
      (format *stderr* "pisum: ~S~%" (pisum))))

  (do ((n 0 (+ n 1)) ; Julia original only runs it once!
       (a (make-float-vector 5000) (make-float-vector 5000)))
      ((= n 10))
    (do ((i 0 (+ i 1)))
	((= i 5000))
      (set! (a i) (random 5000.0)))
    (quicksort a 0 4999)
    (when (zero? n)
      ;; make sure it worked...
      (do ((i 1 (+ i 1)))
	  ((= i 5000))
	(if (< (a i) (a (- i 1)))
	    (display "."))))))

(jtests)


(define (mean v)
  (let ((len (length v))
	(sum 0))
    (do ((i 0 (+ i 1)))
	((= i len)
	 (/ sum len))
      (set! sum (+ sum (v i))))))

(define (medium v)
  (let ((len (length v)))
    (quicksort v 0 (- len 1))
    (if (odd? len)
	(v (ceiling (/ len 2)))
	(/ (+ (v (/ len 2)) (v (+ (/ len 2) 1))) 2))))

(define (mtests)
  (let ((v (make-float-vector 5000)))
    (do ((n 0 (+ n 1)))
	((= n 10))
      (do ((i 0 (+ i 1)))
	  ((= i 5000))
	(set! (v i) (- (random 2.0) 1.0)))
      (mean v) 
      (medium v))))

(mtests)


(define (gammln xx)			;Ln(gamma(xx)), xx>0 
  (let* ((stp 2.5066282746310005)
	 (x xx)
	 (tmp (+ x 5.5))
	 (tmp1 (- tmp (* (+ x 0.5) (log tmp))))
	 (ser (+ 1.000000000190015
		 (/ 76.18009172947146 (+ x 1.0))
		 (/ -86.50532032941677 (+ x 2.0))
		 (/ 24.01409824083091 (+ x 3.0))
		 (/ -1.231739572450155 (+ x 4))
		 (/ 0.1208650973866179e-2 (+ x 5.0))
		 (/ -0.5395239384953e-5 (+ x 6.0)))))
    (- (log (/ (* stp ser) x)) tmp1)))

(define (gser a x)			;P(a,x) evaluated as series, also Ln(gamma)
  (if (< x 0.0) 
      (format #f "~F is less than 0" x)
      (if (= x 0.0) 
	  0.0
	  (let* ((gln (gammln a))
		 (itmax 100)
		 (eps 3.0e-7)
		 (ap a)
		 (sum (/ 1.0 a))
		 (del sum))
	    (do ((n 1 (+ n 1)))
		((or (> n itmax)
		     (< (abs del) (* (abs sum) eps)))
		 (* sum (exp (- (* a (log x)) x gln))))
	      (set! ap (+ ap 1))
	      (set! del (* del (/ x ap)))
	      (set! sum (+ sum del)))))))

(define (gcf a x)			;Q(a,x) evaluated as continued fraction
  (let ((itmax 100)
	(eps 3.0e-7)
	(gln (gammln a))
	(gold 0.0)			;previous value of g, tested for convergence
	(a0 1.0)
	(a1 x)
	(b0 0.0)
	(b1 1.0)			;setting up continued fraction
	(fac 1.0)
	(ana 0.0) (g 0.0) (anf 0.0))
    (call-with-exit
     (lambda (return)
       (do ((n 1 (+ n 1)))
	   ((> n itmax) 
	    (* g (exp (- (* a (log x)) x gln))))
	 (set! ana (- n a))
	 (set! a0 (* fac (+ a1 (* a0 ana))))
	 (set! b0 (* fac (+ b1 (* b0 ana))))
	 (set! anf (* fac n))
	 (set! a1 (+ (* x a0) (* anf a1)))
	 (set! b1 (+ (* x b0) (* anf b1)))
	 (unless (= 0.0 a1)		;renormalize?
	   (set! fac (/ 1.0 a1))
	   (set! g (* b1 fac))
	   (if (< (abs (/ (- g gold) g)) eps)
	       (return (* g (exp (- (* a (log x)) x gln)))))))))))
	 
(define (gammp a x)			; incomplete gamma function P(a,x)
  (if (or (<= a 0.0) 
	  (< x 0.0))
      (format #f "Invalid argument to gammp: ~F" (if (<= 0.0 a) a x))
      (if (< x (+ a 1.0))
	  (gser a x)			;use series
	  (- 1.0 (gcf a x)))))		;use continued fraction
  
(define (erf x)			        ; error function erf(x)
  (if (< x 0.0)
      (- (gammp 0.5 (* x x)))
      (gammp 0.5 (* x x))))

(define (gammq a x)                     ;incomplete gamma function Q(a,x) = 1 - P(a,x)
  (- 1.0 (gammp a x)))

(define (erfc x)                        ;complementary error function erfc(x)
  (if (< x 0.0)
      (+ 1.0 (gammp 0.5 (* x x)))
      (gammq 0.5 (* x x))))

(define (test-erf)
  (let-temporarily (((*s7* 'equivalent-float-epsilon) 1e-12))
    (unless (equivalent? (erf 0) 0.0) (format *stderr* "erf 0: ~S~%" (erf 0)))
    (unless (equivalent? (erf 1) 0.8427007900291826) (format *stderr* "erf 1: ~S~%" (erf 1)))
    (unless (equivalent? (erfc 1) 0.15729920997081737) (format *stderr* "erfc 1: ~S~%" (erfc 1)))
    (unless (equivalent? (erf 0.5) 0.5204998760683841) (format *stderr* "erf 0.5: ~S~%" (erf 0.5)))
    (unless (equivalent? (erf 2) 0.9953222650189529) (format *stderr* "erf 2: ~S~%" (erf 2)))
    (unless (equivalent? (erf 0.35) 0.3793820529938486) (format *stderr* "erf 0.35: ~S~%" (erf 0.35)))

    (do ((i 0 (+ i 1))
	 (x 0.0 (+ x .001)))
	((= i 3000))
      (unless (equivalent? (+ (erf x) (erfc x)) 1.0)
	(format *stderr* "erf: ~S trouble\n" x)))))

(test-erf)


(define show-digits-of-pi-starting-at-digit
  ;; piqpr8.c
  ;;    This program implements the BBP algorithm to generate a few hexadecimal
  ;;    digits beginning immediately after a given position id, or in other words
  ;;    beginning at position id + 1.  On most systems using IEEE 64-bit floating-
  ;;    point arithmetic, this code works correctly so long as d is less than
  ;;    approximately 1.18 x 10^7.  If 80-bit arithmetic can be employed, this limit
  ;;    is significantly higher.  Whatever arithmetic is used, results for a given
  ;;    position id can be checked by repeating with id-1 or id+1, and verifying 
  ;;    that the hex digits perfectly overlap with an offset of one, except possibly
  ;;    for a few trailing digits.  The resulting fractions are typically accurate 
  ;;    to at least 11 decimal digits, and to at least 9 hex digits.  
  ;;  David H. Bailey     2006-09-08

  (let ((ihex (lambda (x nhx chx)
		;; This returns, in chx, the first nhx hex digits of the fraction of x.
		(do ((y (abs x))
		     (hx "0123456789ABCDEF")
		     (i 0 (+ i 1)))
		    ((= i nhx) chx)
		  (set! y (* 16.0 (- y (floor y))))
		  (set! (chx i) (hx (floor y))))))
	(series (lambda (m id)
		  ;; This routine evaluates the series sum_k 16^(id-k)/(8*k+m) using the modular exponentiation technique.
		  (let ((expm (let ((ntp 25))
				(let ((tp1 0)
				      (tp (make-vector ntp)))
				  (lambda (p ak)
				    ;; expm = 16^p mod ak.  This routine uses the left-to-right binary exponentiation scheme.
				    ;; If this is the first call to expm, fill the power of two table tp.
				    (when (= tp1 0)
				      (set! tp1 1)
				      (set! (tp 0) 1.0)
				      (do ((i 1 (+ i 1)))
					  ((= i ntp))
					(set! (tp i) (* 2.0 (tp (- i 1))))))
				    
				    (if (= ak 1.0)
					0.0
					(let ((pl -1))
					  ;;  Find the greatest power of two less than or equal to p.
					  (do ((i 0 (+ i 1)))
					      ((or (not (= pl -1)) 
						   (= i ntp)))
					    (if (> (tp i) p)
						(set! pl i)))
					  
					  (if (= pl -1) (set! pl ntp))
					  (let ((pt (tp (- pl 1)))
						(p1 p)
						(r 1.0))
					    ;;  Perform binary exponentiation algorithm modulo ak.
					    
					    (do ((j 1 (+ j 1)))
						((> j pl) r)
					      (when (>= p1 pt)
						(set! r (* 16.0 r))
						(set! r (- r (* ak (floor (/ r ak)))))
						(set! p1 (- p1 pt)))
					      (set! pt (* 0.5 pt))
					      (when (>= pt 1.0)
						(set! r (* r r))
						(set! r (- r (* ak (floor (/ r ak))))))))))))))
			(eps 1e-17)
			(s 0.0))
		    (do ((k 0 (+ k 1)))
			((= k id))
		      (let* ((ak (+ (* 8 k) m))
			     (t (expm (- id k) ak)))
			(set! s (+ s (/ t ak)))
			(set! s (- s (floor s)))))
		    
		    ;; Compute a few terms where k >= id.
		    (do ((happy #f)
			 (k id (+ k 1)))
			((or (> k (+ id 100)) happy) s)
		      (let ((t (/ (expt 16.0 (- id k)) (+ (* 8 k) m))))
			(set! happy (< t eps))
			(set! s (+ s t))
			(set! s (- s (floor s)))))))))
    (lambda (id)  
      ;; id is the digit position.  Digits generated follow immediately after id.
      (let ((chx (make-string 10 #\space))
	    (pid (let ((s1 (series 1 id))
		       (s2 (series 4 id))
		       (s3 (series 5 id))
		       (s4 (series 6 id)))
		   (- (+ (* 4.0 s1) (* -2.0 s2)) s3 s4))))
	(ihex (- (+ 1.0 pid) (floor pid)) 10 chx)
	chx))))

(define (test-digits)
  (do ((i 0 (+ i 1)))
      ((= i 5))
    (show-digits-of-pi-starting-at-digit (* i 1000))))

(test-digits)


(exit)

