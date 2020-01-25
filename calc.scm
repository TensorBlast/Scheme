(define calc #f)

(let ()
  (define do-calc
    (lambda (ek expr)
      (cond
       ((number? expr) expr)
       ((and (list? expr) (= (length expr) 2))
	(let ((op (car expr)) (args (cdr expr)))
	  (case op
	    ((minus) (apply-op1 ek - args))
	    ((sqrt) (apply-op1 ek sqrt args))
	    (else (complain ek "Invalid unary operator" op)))))
       ((and (list? expr) (= (length expr) 3))
	(let ((op (car expr)) (args (cdr expr)))
	  (case op
	    ((add) (apply-op ek + args))
	    ((sub) (apply-op ek - args))
	    ((mul) (apply-op ek * args))
	    ((div) (apply-op ek / args))
	    (else (complain ek "Invalid operator" op)))))
       (else (complain ek "Invlid expression" expr)))))

  (define apply-op1
    (lambda (ek op args)
      (op (do-calc ek (car args)))))
  
(define apply-op
  (lambda (ek op args)
    (op (do-calc ek (car args)) (do-calc ek (cadr args)))))

(define complain
  (lambda (ek msg expr)
    (ek (list msg expr))))

(set! calc
  (lambda (expr)
    (call/cc
     (lambda (ek)
       (do-calc ek expr))))))
