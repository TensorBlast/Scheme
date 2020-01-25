#lang racket

(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))


(define rember*
  (lambda (a l)
          (cond
            ((null? l) (quote ()))
            ((atom? (car l))
            (cond
              ((eq? a (car l)) (rember* a (cdr l)))
              (else (cons (car l) (rember* a (cdr l))))))
            (else (cons (rember* a (car l)) (rember* a (cdr l))))
            )
          )
  )