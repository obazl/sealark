(define times 1000)
(define size 1024)

(define (b_fft areal aimag)
  (let ((ar 0)
        (ai 0)
        (i 1)
        (j 1)
        (k 0)
        (m 0)
        (n 0)
        (le 1)
        (le1 0) (le2 0)
        (ip 0)
        (nv2 0)
        (nm1 0)
        (ur 0.0)
        (ui 0.0)
        (wr 0.0)
        (wi 0.0)
        (tr 0.0)
        (ti 0.0))
    ;; initialize
    (set! ar areal)
    (set! ai aimag)
    (set! n (length ar))
    (set! n (- n 1))
    (set! nv2 (quotient n 2))
    (set! nm1 (- n 1))
    
    (do ()
	((>= i n))
      (set! m (+ m 1))
      (set! i (+ i i)))
    (set! i 1)
    
    (do ()
	((>= i n))
      (when (< i j)
	(set! tr (float-vector-ref ar j))
	(set! ti (float-vector-ref ai j))
	(float-vector-set! ar j (float-vector-ref ar i))
	(float-vector-set! ai j (float-vector-ref ai i))
	(float-vector-set! ar i tr)
	(float-vector-set! ai i ti))
      (set! k nv2)
      (do ()
	  ((>= k j))
	(set! j (- j k))
	(set! k (quotient k 2)))
      
      (set! j (+ j k))
      (set! i (+ i 1)))
    
    (do ((l 1 (+ l 1)))
        ((> l m))  
      (set! le1 le)
      (set! le2 (+ le 1))
      (set! le (* le 2))
      (set! ur 1.0)
      (set! ui 0.)
      (set! wr (cos (/ pi le1)))
      (set! wi (sin (/ pi le1)))
      (do ((j1 1 (+ j1 1)))
	  ((= j1 le2))
	(do ((i1 j1 (+ i1 le)))
	    ((> i1 n))
	  (set! ip (+ i1 le1))
	  (set! tr (- (* (float-vector-ref ar ip) ur)
		      (* (float-vector-ref ai ip) ui)))
	  (set! ti (+ (* (float-vector-ref ar ip) ui)
		      (* (float-vector-ref ai ip) ur)))
	  (float-vector-set! ar ip (- (float-vector-ref ar i1) tr))
	  (float-vector-set! ai ip (- (float-vector-ref ai i1) ti))
	  (float-vector-set! ar i1 (+ (float-vector-ref ar i1) tr))
	  (float-vector-set! ai i1 (+ (float-vector-ref ai i1) ti))))
      (set! tr (- (* ur wr) (* ui wi)))
      (set! ti (+ (* ur wi) (* ui wr)))
      (set! ur tr)
      (set! ui ti))
    #t))
 

;;; --------------------------------------------------------------------------------
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
        ((or (< m 2) 
             (< j m))
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
           (j (+ ii prev) (+ j mmax)))
          ((>= jj pow))
        (let ((tc (* wc (data j))))
          (set! (data j) (- (data i) tc))
          (set! (data i) (+ (data i) tc))))
      (set! wc (* wc wpc))))
  data)

(unless (equivalent? (cfft (vector 0.0 1+i 0.0 0.0)) #(1+1i -1+1i -1-1i 1-1i))
  (format *stderr* "cfft 1: ~S~%" (cfft (vector 0.0 1+i 0.0 0.0))))
(let-temporarily (((*s7* 'equivalent-float-epsilon) 1e-14))
  (unless (equivalent? (cfft (vector 0 0 1+i 0 0 0 1-i 0)) #(2 -2 -2 2 2 -2 -2 2))
    (format *stderr* "cfft 2: ~S~%" (cfft (vector 0 0 1+i 0 0 0 1-i 0)))))


;;; --------------------------------------------------------------------------------
(define (fft-bench)
  (let ((*re* (make-float-vector (+ size 1) 0.10))
	(*im* (make-float-vector (+ size 1) 0.10)))
    (do ((ntimes 0 (+ ntimes 1)))
	((= ntimes times))
      (b_fft *re* *im*)))

  (let* ((n 256)
	 (cdata (make-vector n 0.0)))
    (do ((i 0 (+ i 1)))
	((= i times))
      (fill! cdata 0.0)
      (vector-set! cdata 2 1+i)
      (vector-set! cdata (- n 1) 1-i)
      (cfft cdata))))
 
(fft-bench)


;;;--------------------------------------------------------------------------------
;; 2D dft, real data, spot-checked against fftw ("backward", for "forward" case, use (- (im yout xout)...))
(define (dft2 in)
  (let* ((dims (vector-dimensions in))
         (h (car dims))
         (w (cadr dims))
	 (ih (/ (* 2.0 pi) h))
	 (iw (/ (* 2.0 pi) w))
         (rl (make-float-vector dims 0.0))
         (im (make-float-vector dims 0.0))
	 (out (make-float-vector dims))
	 (xw 0)
	 (yh 0))
    (do ((yout 0 (+ yout 1)))
        ((= yout h) 
	 (do ((i 0 (+ i 1)))
	     ((= i h) 
	      (list rl im out))
	   (do ((j 0 (+ j 1)))
	       ((= j w))
	     (set! (out i j) (+ (* (rl i j) (rl i j)) 
				(* (im i j) (im i j)))))))
      (do ((xout 0 (+ xout 1)))
          ((= xout w))
	(set! xw (* xout iw))
        (do ((yin 0 (+ yin 1)))
            ((= yin h))
	  (set! yh (* yout yin ih))
          (do ((xin 0 (+ xin 1)))
              ((= xin w))
            (set! (rl yout xout) (+ (rl yout xout) 
				    (* (in yin xin) 
				       (cos (+ yh (* xw xin))))))
            (set! (im yout xout) (+ (im yout xout) 
				    (* (in yin xin) 
				       (sin (+ yh (* xw xin))))))))))))

(let ((vs (dft2 #r2d((1.0 0.0 -1.0 0.0)
		     (0.0 0.0 0.0 0.0)
		     (0.0 1.0 0.0 0.0)
		     (0.0 0.0 0.0 0.0)))))
  (let-temporarily (((*s7* 'equivalent-float-epsilon) 1.0e-14))
    (unless (equivalent? (car vs) #r2d((1.0 2.0 -1.0 2.0) (-1.0 2.0 1.0 2.0) (1.0 2.0 -1.0 2.0) (-1.0 2.0 1.0 2.0)))
      (format *stderr* "dft2 rl: ~S~%" (car vs)))
    (unless (equivalent? (cadr vs) #r2d((0.0 1.0 0 -1.0) (0.0 -1.0 0.0 1.0) (0.0 1.0 0.0 -1.0) (0.0 -1.0 0.0 1.0)))
      (format *stderr* "dft2 im: ~S~%" (cadr vs)))
    (unless (equivalent? (caddr vs) #r2d((1.0 5.0 1.0 5.0) (1.0 5.0 1.0 5.0) (1.0 5.0 1.0 5.0) (1.0 5.0 1.0 5.0)))
      (format *stderr* "dft2 out: ~S~%" (caddr vs)))))

(define (test-dft2)
  (let ((mat (make-float-vector '(16 16) 0.0)))
    (set! (mat 0 8) 1.0)
    (dft2 mat)
    (fill! mat 0.0)
    (set! (mat 0 0) 1.0)
    (set! (mat 8 8) 1.0)
    (dft2 mat)
    (fill! mat 0.0)
    (set! (mat 0 0) 1.0)
    (set! (mat 4 4) 0.5)
    (set! (mat 8 8) 1.0)
    (dft2 mat)
    (fill! mat 0.0)
    (set! (mat 0 0) 1.0)
    (set! (mat 2 0) 0.5)
    (set! (mat 8 8) 1.0)
    (dft2 mat)))

(test-dft2)


;;; --------------------------------------------------------------------------------
;; 3D dft as above but the "forward" case
(define (dft3 in)
  (let* ((dims (vector-dimensions in))
         (h (car dims))
         (w (cadr dims))
	 (d (caddr dims))
	 (ih (/ (* 2.0 pi) h))
	 (iw (/ (* 2.0 pi) w))
	 (id (/ (* 2.0 pi) d))
         (rl (make-float-vector dims))
         (im (make-float-vector dims))
	 (out (make-float-vector dims)))
    (do ((yout 0 (+ yout 1)))
        ((= yout h) 
	 (do ((i 0 (+ i 1)))
	     ((= i h) 
	      (list rl im out))
	   (do ((j 0 (+ j 1)))
	       ((= j w))
	     (do ((k 0 (+ k 1)))
	       ((= k d))
	       (set! (out i j k) 
		     (+ (* (rl i j k) (rl i j k)) 
			(* (im i j k) (im i j k))))))))
      (do ((xout 0 (+ xout 1)))
          ((= xout w))
	(do ((zout 0 (+ zout 1)))
	    ((= zout d))
	  (do ((yin 0 (+ yin 1)))
	      ((= yin h))
	    (do ((xin 0 (+ xin 1)))
		((= xin w))
	      (do ((zin 0 (+ zin 1)))
		  ((= zin d))
		(set! (rl yout xout zout)
		      (+ (rl yout xout zout) 
			 (* (in yin xin zin) 
			    (cos (+ (* xout xin iw) 
				    (* yout yin ih)
				    (* zout zin id))))))
		(set! (im yout xout zout)
		      (- (im yout xout zout) 
			 (* (in yin xin zin) 
			    (sin (+ (* xout xin iw) 
				    (* yout yin ih)
				    (* zout zin id))))))))))))))

(let ((vs (dft3 #r3d(((1.0 0.5 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
		     ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
		     ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
		     ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))))))
  (let-temporarily (((*s7* 'equivalent-float-epsilon) 1.0e-14)
                    ((*s7* 'print-length) 128))
    (unless (equivalent? (car vs)
			 #r3d(((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0))
			       ((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0)) 
			       ((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0))
			       ((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0))))
      (format *stderr* "dft3 rl: ~S~%" (car vs)))
    (unless (equivalent? (cadr vs) 
			 #r3d(((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))
			      ((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))
			      ((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))
			      ((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))))
      (format *stderr* "dft3 im: ~S~%" (cadr vs)))
    (unless (equivalent? (caddr vs) 
			 #r3d(((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25))
			      ((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25))
			      ((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25)) 
			      ((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25))))
      (format *stderr* "dft3 out: ~S~%" (caddr vs)))))

(define (test-dft3)
  (let ((mat (make-float-vector '(8 8 8) 0.0)))
    ;(set! (mat 0 4 0) 1.0)
    ;(dft3 mat)
    ;(fill! mat 0.0)
    (set! (mat 0 0 0) 1.0)
    (set! (mat 4 4 4) 1.0)
    (dft3 mat)))

(test-dft3)

;;; explicit case
(define (dft3ex in)
  (let* ((dims (vector-dimensions in))
         (h (car dims))
         (w (cadr dims))
	 (d (caddr dims))
	 (ih (/ (* 2.0 pi) h))
	 (iw (/ (* 2.0 pi) w))
	 (id (/ (* 2.0 pi) d))
         (rl (make-float-vector dims))
         (im (make-float-vector dims))
	 (out (make-float-vector dims)))
    (do ((yout 0 (+ yout 1)))
        ((= yout h) 
	 (do ((i 0 (+ i 1)))
	     ((= i h) 
	      (list rl im out))
	   (do ((j 0 (+ j 1)))
	       ((= j w))
	     (do ((k 0 (+ k 1)))
	       ((= k d))
	       (float-vector-set! out i j k
		     (+ (* (float-vector-ref rl i j k) (float-vector-ref rl i j k)) 
			(* (float-vector-ref im i j k) (float-vector-ref im i j k))))))))
      (do ((xout 0 (+ xout 1)))
          ((= xout w))
	(do ((zout 0 (+ zout 1)))
	    ((= zout d))
	  (do ((yin 0 (+ yin 1)))
	      ((= yin h))
	    (do ((xin 0 (+ xin 1)))
		((= xin w))
	      (do ((zin 0 (+ zin 1)))
		  ((= zin d))
		(float-vector-set! rl yout xout zout
		      (+ (float-vector-ref rl yout xout zout) 
			 (* (float-vector-ref in yin xin zin) 
			    (cos (+ (* xout xin iw) 
				    (* yout yin ih)
				    (* zout zin id))))))
		(float-vector-set! im yout xout zout
		      (- (float-vector-ref im yout xout zout) 
			 (* (float-vector-ref in yin xin zin) 
			    (sin (+ (* xout xin iw) 
				    (* yout yin ih)
				    (* zout zin id))))))))))))))

;;; implicit case
(define (dft3ex-1 in)
  (let* ((dims (vector-dimensions in))
         (h (car dims))
         (w (cadr dims))
	 (d (caddr dims))
	 (ih (/ (* 2.0 pi) h))
	 (iw (/ (* 2.0 pi) w))
	 (id (/ (* 2.0 pi) d))
         (rl (make-float-vector dims))
         (im (make-float-vector dims))
	 (out (make-float-vector dims))
	 (zd 0)
	 (xw 0)
	 (yh 0))
    (do ((yout 0 (+ yout 1)))
        ((= yout h) 
	 (do ((i 0 (+ i 1)))
	     ((= i h) 
	      (list rl im out))
	   (do ((j 0 (+ j 1)))
	       ((= j w))
	     (do ((k 0 (+ k 1)))
	       ((= k d))
	       (set! (out i j k)
		     (+ (* (rl i j k) (rl i j k)) 
			(* (im i j k) (im i j k))))))))
      (do ((xout 0 (+ xout 1)))
          ((= xout w))
	(do ((zout 0 (+ zout 1)))
	    ((= zout d))
	  (set! zd (* zout id))
	  (do ((yin 0 (+ yin 1)))
	      ((= yin h))
	    (set! yh (* yout yin ih))
	    (do ((xin 0 (+ xin 1)))
		((= xin w))
	      (set! xw (+ yh (* xout xin iw)))
	      (do ((zin 0 (+ zin 1)))
		  ((= zin d))
		(set! (rl yout xout zout)
		      (+ (rl yout xout zout) 
			 (* (in yin xin zin)
			    (cos (+ xw (* zin zd))))))
		(set! (im yout xout zout)
		      (- (im yout xout zout) 
			 (* (in yin xin zin)  
			    (sin (+ xw (* zin zd))))))))))))))

(define (check-dft3 name vs)
  (let-temporarily (((*s7* 'equivalent-float-epsilon) 1.0e-14)
                    ((*s7* 'print-length) 128))
    (unless (equivalent? (car vs)
			 #r3d(((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0))
			       ((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0)) 
			       ((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0))
			       ((1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0) (1.5 1.0 0.5 1.0))))
      (format *stderr* "~A rl: ~S~%" name (car vs)))
    (unless (equivalent? (cadr vs) 
			 #r3d(((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))
			      ((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))
			      ((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))
			      ((0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5) (0.0 -0.5 0.0 0.5))))
      (format *stderr* "~A im: ~S~%" name (cadr vs)))
    (unless (equivalent? (caddr vs) 
			 #r3d(((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25))
			      ((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25))
			      ((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25)) 
			      ((2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25) (2.25 1.25 0.25 1.25))))
      (format *stderr* "~A out: ~S~%" name (caddr vs)))))

(check-dft3 "dft3ex" (dft3ex #r3d(((1.0 0.5 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
				  ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
				  ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
				  ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0)))))

(check-dft3 "dft3ex-1" (dft3ex-1 #r3d(((1.0 0.5 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
				      ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
				      ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0))
				      ((0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0) (0.0 0.0 0.0 0.0)))))


(define (test-dft3ex)
  (let ((mat (make-float-vector '(9 9 9) 0.0)))
    (float-vector-set! mat 0 0 0 1.0)
    (dft3ex mat))
  (let ((mat (make-float-vector '(9 9 9) 0.0)))
    (float-vector-set! mat 0 0 0 1.0)
    (dft3ex-1 mat)))

(test-dft3ex)



(when (> (*s7* 'profile) 0)
  (show-profile 200))
(exit)


#|
;;; fft3.c:
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <complex.h>
#include <fftw3.h>

static fftw_complex *c_in_data = NULL, *c_out_data = NULL;
static fftw_plan c_r_plan, c_i_plan;  
static int last_c_fft_size = 0;   

static void mus_fftw_with_imag(double *rl, double *im, int h, int w, int d, int dir)
{
  int i, j, k;

  c_in_data = (fftw_complex *)fftw_malloc(w * h * d * sizeof(fftw_complex)); /* rl/im data is double */
  c_out_data = (fftw_complex *)fftw_malloc(w * h * d * sizeof(fftw_complex));
  c_r_plan = fftw_plan_dft_3d(h, w, d, c_in_data, c_out_data, FFTW_FORWARD, FFTW_ESTIMATE); 
  c_i_plan = fftw_plan_dft_3d(h, w, d, c_in_data, c_out_data, FFTW_BACKWARD, FFTW_ESTIMATE);

  for (i = 0; i < h; i++)
    for (j = 0; j < w; j++)
      for (k = 0; k < d; k++)
	c_in_data[(i * w * d) + (j * d) + k] = rl[(i * w * d) + (j * d) + k] + _Complex_I * im[(i * w * d) + (j * d) + k];

  if (dir == -1) 
    fftw_execute(c_r_plan);
  else fftw_execute(c_i_plan);

  for (i = 0; i < h; i++)
    for (j = 0; j < w; j++)
      for (k = 0; k < d; k++)
	{
	  rl[(i * w * d) + (j * d) + k] = creal(c_out_data[(i * w * d) + (j * d) + k]);
	  im[(i * w * d) + (j * d) + k] = cimag(c_out_data[(i * w * d) + (j * d) + k]);
	}
}

int main(int argc, char **argv)
{
  double *rl, *im;
  int h, w, d, i, j, k;
  h = 4;
  w = 4;
  d = 4;
  rl = (double *)calloc(h * w * d, sizeof(double));
  im = (double *)calloc(h * w * d, sizeof(double));
  
  rl[0] = 1.0;
  rl[1] = 0.5;

  mus_fftw_with_imag(rl, im, h, w, d, -1);

  fprintf(stderr, "#r3d(");
  for (i = 0; i < h; i++)
    {
      fprintf(stderr, "(");
      for (j = 0; j < w; j++)
	{
	  fprintf(stderr, "(");
	  for (k = 0; k < d; k++)
	    fprintf(stderr, "%.3f ", rl[(i * w * d) + (j * d) + k]);
	  fprintf(stderr, ")");
	}
      fprintf(stderr, ")");
    }
  fprintf(stderr, ")\n");

  fprintf(stderr, "#r3d(");
  for (i = 0; i < h; i++)
    {
      fprintf(stderr, "(");
      for (j = 0; j < w; j++)
	{
	  fprintf(stderr, "(");
	  for (k = 0; k < d; k++)
	    fprintf(stderr, "%.3f ", im[(i * w * d) + (j * d) + k]);
	  fprintf(stderr, ")");
	}
      fprintf(stderr, ")");
    }
  fprintf(stderr, ")\n");

  fprintf(stderr, "#r3d(");
  for (i = 0; i < h; i++)
    {
      fprintf(stderr, "(");
      for (j = 0; j < w; j++)
	{
	  fprintf(stderr, "(");
	  for (k = 0; k < d; k++)
	    fprintf(stderr, "%.3f ", 
		    rl[(i * w * d) + (j * d) + k] * rl[(i * w * d) + (j * d) + k] + im[(i * w * d) + (j * d) + k] * im[(i * w * d) + (j * d) + k]);
	  fprintf(stderr, ")");
	}
      fprintf(stderr, ")");
    }
  fprintf(stderr, ")\n");
}
|#
