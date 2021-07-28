;;; string and char timing

(define (concord)
  (call-with-output-file "test.cx"
    (lambda (op)
      (call-with-input-file "s7.c"
	(lambda (ip)
	  (let ((words (make-hash-table))
		(cur-word (make-string 512))
		(word "")
		(cur-loc 0)
		(cur-line 1)
		(last-c #\null))
	    (do ((c (read-char ip) (read-char ip)))
		((eof-object? c)
		 (for-each (lambda (w)
			     (format op "~A: ~S~%" (car w) (reverse (cdr w))))
			   words))
	      (if (or (char-alphabetic? c)
		      (char=? c #\_)
		      (and (char-numeric? c)
			   (positive? (length cur-word))))
		  (begin 
		    (string-set! cur-word cur-loc c)
		    (set! cur-loc (+ cur-loc 1)))
		  (begin
		    (cond ((char=? c #\newline)
			   (set! cur-line (+ cur-line 1)))

			  ((and (char=? c #\*)
				(char=? last-c #\/))
			   (let ((last-c1 #\null))
			     (do ((c1 (read-char ip) (read-char ip)))
				 ((and (char=? c1 #\/)
				       (char=? last-c1 #\*)))
			       (if (char=? c1 #\newline)
				   (set! cur-line (+ cur-line 1)))
			       (set! last-c1 c1))))

			  ((and (char=? c #\")
				(not (char=? last-c #\')))               ; '"' 
			   (let ((last-c1 #\null)
				 (last-c2 #\null))
			     (do ((c1 (read-char ip) (read-char ip)))
				 ((and (char=? c1 #\")
				       (or (not (char=? last-c1 #\\))    ; \"
					   (char=? last-c2 #\\))))       ; \\"
			       (if (char=? c1 #\newline)
				   (set! cur-line (+ cur-line 1)))
			       (set! last-c2 last-c1)
			       (set! last-c1 c1)))))
		    (set! last-c c)
		    (when (positive? cur-loc)
		      (set! word (substring cur-word 0 cur-loc))
		      (hash-table-set! words word ; in non-code text we'd probably want string-downcase here and below
				       (cons cur-line (or (hash-table-ref words word) ()))))
		    (set! cur-loc 0))))))))))

;;; --------------------------------

(define (searcher)
  ;; }\n}
  (call-with-input-file "s7.c"
    (lambda (p)
      (let ((last1 "")
	    (last-i 0))
	(do ((this (read-line p) (read-line p))
	     (line 0 (+ line 1)))
	    ((eq? this #<eof>))
	  (let ((len (length this)))
	    (unless (or (= len 0)
			(char=? (string-ref this 0) #\}))
	      (do ((i 0 (+ i 1)))
		  ((or (>= i len)
		       (not (char-whitespace? (string-ref this i))))
		   (when (and (< i len) ; i.e. not char-whitespace above
			      (char=? (string-ref this i) #\})
			      (> (length last1) 0)
			      (char=? (string-ref last1 last-i) #\}))
		     (format #f "~D ~S~%" line last1))
		   (set! last-i i)
		   (set! last1 this))))))))))


;;; --------------------------------
;;; various simple cases

(define (strcop str) ; opt_dotimes
  (let* ((len (length str))
	 (new-str (make-string len)))
    (do ((i 0 (+ i 1)))
	((= i len) new-str)
      (string-set! new-str i (string-ref str i)))))

(define (strup str)
  (let* ((len (length str))
	 (new-str (make-string len)))
    (do ((i 0 (+ i 1)))
	((= i len) new-str)
      (string-set! new-str i (char-upcase (string-ref str i))))))

(define (let-strup str)
  (let* ((len (length str))
	 (new-str (make-string len)))
    (let loop ((i 0))
      (cond ((= i len) new-str)
	    (else 
	     (string-set! new-str i (char-upcase (string-ref str i)))
	     (loop (+ i 1)))))))


(define tc-cpos ; op_tc_if_a_z_if_a_z_la [opt]
  (let ((len 0)
	(c #f)
	(str #f))
    (define (cpos-1 pos)
      (if (= pos len)
	  #f
	  (if (char=? c (string-ref str pos))
	       pos
	       (cpos-1 (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (cpos-1 0))))

(define rev-cpos  ; op_tc_if_a_z_if_a_la_z [opt, same as direct case]
  (let ((len 0)
	(c #f)
	(str #f))
    (define (cpos-rev pos)
      (if (= pos len)
	  #f
	  (if (not (char=? c (string-ref str pos)))
	      (cpos-rev (+ pos 1))
	      pos)))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (cpos-rev 0))))

(define tc2-cpos ; op_tc_if_a_z_if_a_z_laa
  (let ((len 0)
	(str #f))
    (define (cpos-2 c pos)
      (if (= pos len)
	  #f
	  (if (char=? c (string-ref str pos))
	       pos
	       (cpos-2 c (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! str str1)
      (cpos-2 c1 0))))

(define cond2-cpos ; op_tc_cond_a_z_if_a_z_laa
  (let ((len 0)
	(str #f))
    (define (cond2-cpos-2 c pos)
      (cond ((= pos len) #f)
	    ((char=? c (string-ref str pos)) pos)
	    (else (cond2-cpos-2 c (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! str str1)
      (cond2-cpos-2 c1 0))))

(define cond2-cposrev ; op_tc_cond_a_z_if_a_laa_z
  (let ((len 0)
	(str #f))
    (define (cond2-cposrev-2 c pos)
      (cond ((= pos len) #f)
	    ((not (char=? c (string-ref str pos))) (cond2-cposrev-2 c (+ pos 1)))
	    (else pos)))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! str str1)
      (cond2-cposrev-2 c1 0))))

(define tc3-cpos ; eval? (there is no op_tc_if_a_z_if_a_z_l3a)
  (let ((len 0))
    (define (cpos-3 c str pos)
      (if (= pos len)
	  #f
	  (if (char=? c (string-ref str pos))
	       pos
	       (cpos-3 c str (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (cpos-3 c1 str1 0))))

(define and-cpos 
  (let ((len 0)
	(c #f)
	(str #f))
    (define (and-cpos-1 pos)
      (and (< pos len)
	   (if (char=? c (string-ref str pos))
	       pos
	       (and-cpos-1 (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (and-cpos-1 0))))

(define andrev-cpos 
  (let ((len 0)
	(c #f)
	(str #f))
    (define (andrev-cpos-1 pos)
      (and (< pos len)
	   (if (not (char=? c (string-ref str pos)))
	       (andrev-cpos-1 (+ pos 1))
	       pos)))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (andrev-cpos-1 0))))

(define cond-cpos ; op_tc_if_a_z_if_a_z_la [opt]
  (let ((len 0)
	(c #f)
	(str #f))
    (define (cond-cpos-1 pos)
      (cond ((= pos len)
	     #f)
	    ((char=? c (string-ref str pos))
	     pos)
	    (else (cond-cpos-1 (+ pos 1)))))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (cond-cpos-1 0))))

(define condrev-cpos 
  (let ((len 0)
	(c #f)
	(str #f))
    (define (condrev-cpos-1 pos)
      (cond ((= pos len)
	     #f)
	    ((not (char=? c (string-ref str pos)))
	     (condrev-cpos-1 (+ pos 1)))
	    (else pos)))
    (lambda (c1 str1)
      (set! len (length str1))
      (set! c c1)
      (set! str str1)
      (condrev-cpos-1 0))))

(define (do-cpos c str) ; op_dox
  (do ((len (length str))
       (i 0 (+ i 1)))
      ((or (= i len)
	   (char=? c (string-ref str i)))
       (and (< i len)
	    i))))

(define (let-cpos c str)
  (let ((len (length str)))
    (let loop ((i 0))
      (cond ((= i len) #f)
	    ((char=? c (string-ref str i)) i)
	    (else (loop (+ i 1)))))))

(define (call-cpos c str)
  (call-with-exit
   (lambda (return)
     (do ((len (length str))
	  (i 0 (+ i 1)))
	 ((= i len) #f)
       (if (char=? c (string-ref str i))
	   (return i))))))


(define tc-spos ; op_tc_if_a_z_if_a_z_la, substr+start&end [opt]
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (spos-1 pos)
      (if (= pos slen)
	  #f
	   (if (string=? find (substring str pos (+ pos flen)))
	       pos
	       (spos-1 (+ pos 1)))))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (spos-1 0))))

(define and-spos
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (and-spos-1 pos)
      (and (< pos slen)
	   (if (string=? find (substring str pos (+ pos flen)))
	       pos
	       (and-spos-1 (+ pos 1)))))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (and-spos-1 0))))

(define andrev-spos
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (andrev-spos-1 pos)
      (and (< pos slen)
	   (if (not (string=? find (substring str pos (+ pos flen))))
	       (andrev-spos-1 (+ pos 1))
	       pos)))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (andrev-spos-1 0))))

(define cond-spos ; op_tc_if_a_z_if_a_z_la [opt]
  (let ((len 0)
	(flen 0)
	(slen 0)
	(find #f)
	(str #f))
    (define (cond-spos-1 pos)
      (cond ((= pos slen)
	     #f)
	    ((string=? find (substring str pos (+ pos flen)))
	     pos)
	    (else (cond-spos-1 (+ pos 1)))))
    (lambda (find1 str1)
      (set! len (length str1))
      (set! flen (length find1))
      (set! slen (- len flen -1))
      (set! str str1)
      (set! find find1)
      (cond-spos-1 0))))

(define (do-spos find str)
  (let ((len (length str))
	(flen (length find)))
    (do ((slen (- len flen -1))
	 (i 0 (+ i 1)))
	((or (= i slen)
	     (string=? find (substring str i (+ i flen))))
	 (and (< i slen)
	      i)))))

(define (call-spos find str)
  (let* ((len (length str))
	 (flen (length find))
	 (slen (- len flen -1)))
    (call-with-exit
     (lambda (return)
       (do ((i 0 (+ i 1)))
	   ((= i slen) #f)
	 (if (string=? find (substring str i (+ i flen)))
	     (return i)))))))


(define (char-count c str)
  (do ((pos (char-position c str 0) (char-position c str (+ pos 1)))
       (count 0 (+ count 1)))
      ((not pos) count)))

(define (do-count c str)
  (let ((len (length str)))
    (do ((i 0 (+ i 1))
	 (count 0))
	((= i len) count)
      (if (char=? c (string-ref str i))
	  (set! count (+ count 1))))))

(define tc-count
  (let ((c #f)
	(str #f)
	(len 0))
    (define (tc-count-1 pos count)
      (if (= pos len)
	  count
	  (tc-count-1 (+ pos 1)
		      (if (char=? c (string-ref str pos)) (+ count 1) count))))
    (lambda (c1 str1)
      (set! c c1)
      (set! str str1)
      (set! len (length str1))
      (tc-count-1 0 0))))

(define (let-count c str)
  (let ((len (length str)))
    (let loop ((pos 0) (count 0))
      (if (= pos len)
	  count
	  (loop (+ pos 1)
		(if (char=? c (string-ref str pos)) (+ count 1) count))))))
    


(let ((val (strcop "asdfghjkl")))
  (unless (string=? val "asdfghjkl") (format *stderr* "strcop ~S: ~S~%" "asdfghjkl" val))
  (set! val (strup "abcdefghij"))
  (unless (string=? val "ABCDEFGHIJ") (format *stderr* "strup ~S: ~S~%" "abcdefghij" val))
  (set! val (let-strup "abcdefghij"))
  (unless (string=? val "ABCDEFGHIJ") (format *stderr* "let-strup ~S: ~S~%" "abcdefghij" val))

  (set! val (tc-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "tc-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (tc2-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "tc2-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (cond2-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "cond2-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (cond2-cposrev #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "cond2-cposrev ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (tc3-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "tc3-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (do-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "do-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (let-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "let-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (and-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "and-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (andrev-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "andrev-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (call-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "call-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (cond-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "cond-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (condrev-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "condrev-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))

  (set! val (rev-cpos #\a "123456789a12343"))
  (unless (eqv? val 9) (format *stderr* "rev-cpos ~C ~S: ~S~%" #\a "123456789a12343" val))


  (set! val (tc-spos "asdf" "fdsghjkasdfhjgfrkl"))
  (unless (eqv? val 7) (format *stderr* "tc-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdfhjgfrkl" val))

  (set! val (do-spos "asdf" "fdsghjkasdfhjgfrkl"))
  (unless (eqv? val 7) (format *stderr* "do-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdfhjgfrkl" val))

  (set! val (and-spos "asdf" "fdsghjkasdfhjgfrkl"))
  (unless (eqv? val 7) (format *stderr* "and-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdfhjgfrkl" val))

  (set! val (tc-spos "asdf" "fdsghjkasdf"))
  (unless (eqv? val 7) (format *stderr* "tc-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" val))

  (set! val (do-spos "asdf" "fdsghjkasdf"))
  (unless (eqv? val 7) (format *stderr* "do-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" val))

  (set! val (and-spos "asdf" "fdsghjkasdf"))
  (unless (eqv? val 7) (format *stderr* "and-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" val))

  (set! val (andrev-spos "asdf" "fdsghjkasdf"))
  (unless (eqv? val 7) (format *stderr* "andrev-spos ~S ~S: ~S~%" "asdf" "fdsghjkasdf" val))

  (set! val (tc-spos "asdf" "fdsghjkasd"))
  (when val (format *stderr* "tc-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" val))

  (set! val (do-spos "asdf" "fdsghjkasd"))
  (when val (format *stderr* "do-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" val))

  (set! val (and-spos "asdf" "fdsghjkasd"))
  (when val (format *stderr* "and-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" val))

  (set! val (andrev-spos "asdf" "fdsghjkasd"))
  (when val (format *stderr* "andrev-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" val))

  (set! val (call-spos "asdf" "fdsghjkasd"))
  (when val (format *stderr* "call-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" val))

  (set! val (cond-spos "asdf" "fdsghjkasd"))
  (when val (format *stderr* "cond-spos ~S ~S: ~S~%" "asdf" "fdsghjkasd" val)))


(define-macro (time . expr) 
  `(let ((start (*s7* 'cpu-time)))
     (do ((k 0 (+ k 1))) ((= k 4)) ,@expr)
     (- (*s7* 'cpu-time) start)))

(newline *stderr*)

(define (simple-tests size)
  (let ((bigstr (make-string size)))
    (do ((i 0 (+ i 1)))
	((= i size))
      (string-set! bigstr i (integer->char (+ 33 (random 94)))))
    (string-set! bigstr (- size 10) #\space)
    (string-set! bigstr (- size 9) #\a)

    (let ((t1 (time (strup bigstr)))
	  (t2 (time (string-upcase bigstr)))
	  (t3 (time (let-strup bigstr))))
      (format *stderr* "strup: ~D ~D~%" (round (/ t1 t2)) (round (/ t3 t2)))

      (set! t1 (time (strcop bigstr)))
      (set! t2 (time (copy bigstr)))
      (format *stderr* "strcop: ~D~%" (round (/ t1 t2)))

      (set! t2 (* 0.5 (time (char-position #\space bigstr) (char-position #\space bigstr))))
      (set! t1 (time (do-cpos #\space bigstr)))
      (format *stderr* "do-cpos: ~D~%" (round (/ t1 t2)))


      (set! t1 (time (tc-cpos #\space bigstr)))
      (format *stderr* "tc-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (tc2-cpos #\space bigstr)))
      (format *stderr* "tc2-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (cond2-cpos #\space bigstr)))
      (format *stderr* "cond2-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (cond2-cposrev #\space bigstr)))
      (format *stderr* "cond2-cposrev: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (tc3-cpos #\space bigstr)))
      (format *stderr* "tc3-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (and-cpos #\space bigstr)))
      (format *stderr* "and-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (andrev-cpos #\space bigstr)))
      (format *stderr* "andrev-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (call-cpos #\space bigstr)))
      (format *stderr* "call-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (cond-cpos #\space bigstr)))
      (format *stderr* "cond-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (condrev-cpos #\space bigstr)))
      (format *stderr* "condrev-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (rev-cpos #\space bigstr)))
      (format *stderr* "rev-cpos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (let-cpos #\space bigstr)))
      (format *stderr* "let-cpos: ~D~%" (round (/ t1 t2)))


      (set! t2 (* 0.5 (time (string-position " a" bigstr) (string-position " a" bigstr))))
      (set! t1 (time (tc-spos " a" bigstr)))
      (format *stderr* "tc-spos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (do-spos " a" bigstr)))
      (format *stderr* "do-spos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (and-spos " a" bigstr)))
      (format *stderr* "and-spos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (andrev-spos " a" bigstr)))
      (format *stderr* "andrev-spos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (call-spos " a" bigstr)))
      (format *stderr* "call-spos: ~D~%" (round (/ t1 t2)))

      (set! t1 (time (cond-spos " a" bigstr)))
      (format *stderr* "cond-spos: ~D~%" (round (/ t1 t2)))

      (let ((c1 0) (c2 0) (c3 0) (c4 0) (t3 0) (t4 0))
	(set! t1 (time (set! c1 (char-count #\a bigstr))))
	(set! t2 (time (set! c2 (do-count #\a bigstr))))
	(set! t3 (time (set! c3 (tc-count #\a bigstr))))
	(set! t4 (time (set! c4 (let-count #\a bigstr))))
	(format *stderr* "counts: ~S ~S ~S ~S, times: ~D ~D ~D~%" 
		c1 c2 c3 c4 
		(round (/ t2 t1)) (round (/ t3 t1)) (round (/ t4 t1)))))

    (do ((i 0 (+ i 1)))
	((= i 20))
      (strup bigstr)
      (string-upcase bigstr)
      (tc-cpos #\space bigstr)
      (do-cpos #\space bigstr)
      (char-position #\space bigstr)
      (tc-spos " a" bigstr)
      (do-spos " a" bigstr)
      (string-position " a" bigstr)
      (strcop bigstr)
      (copy bigstr))))


;;; --------------------------------

(concord)
(simple-tests 100000)
(searcher)

(#_exit)

