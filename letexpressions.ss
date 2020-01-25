
#lang racket

(let ([x 1])
	(let ([new-x (+ x 1)])
		(+ new-x new-x)))

(let ((f (lambda (x . y ) y)))
  (f 'a))

(let ([h (lambda (x y . z) (list x y z)) ] )
  (h 'a 'b 'c 'd))


(define atom?
  (lambda (x)
    (or (null? x) (not (pair? x)))))