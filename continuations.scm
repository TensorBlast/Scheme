(define lwp-list '())

(define lwp
  (lambda (t)
    ((set! lwp-list (append lwp-list (list t))))))

(define start
  (lambda ()
  (let ((p (car lwp-list)))
    (set! lwp-list (cdr lwp-list))
    (p)
    )))

(define pause
  (lambda ()
    (call/cc (lambda (k)
	       (lwp (lambda() (k #f)))
	       (start)))))

(define infinite
  (lambda ()
    (let ((k.n (call/cc (lambda (k) (cons k 0)))))
  (let ((k (car k.n)) (n (cdr k.n)))
    (write n)
    (newline)
    (k (cons k (+ n 1)))))))

(define product
  (lambda (ls)
	  (if (null? ls) 1
	      (if (eq? (car ls) 0) 0
		  (let ((n (product (cdr ls))))
		    (if (= n 0) 0
			(* n (car ls))))))))
