(define fib
  (memoize
     (lambda (n)
	     (if (= n 0) 0
		 (do ((i n (- i 1)) (a1 1 (+ a1 a2)) (a2 0 a1))
		     ((= i 0) a1))))))

(define memoize
  (lambda (proc)
    (let ((cache '()))
      (lambda (n)
	(cond
	 ((assq n cache) => cdr)
	 (else
	  (let ((ans (proc n)))
	    (set! cache (cons (cons n ans) cache))
	    ans)))))))
