;;; xxd but in a way that mimimizes diffs by following the original's line lengths

(call-with-output-file "nrepl-bits.h"
  (lambda (op)
    (call-with-input-file "nrepl.scm"
      (lambda (ip)
	(format op "unsigned char nrepl_scm[] = {~%  ")
	(do ((c (read-char ip) (read-char ip))
	     (i 0 (+ i 1)))
	    ((eof-object? c)
	     (format op "0};~%unsigned int nrepl_scm_len = ~D;~%" i))
	  (format op "0x~X, " (char->integer c))
	  (if (char=? c #\newline)
	      (format op "~%  ")))))))


