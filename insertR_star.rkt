#lang racket

(define atom?
  (lambda (x)
          (and (not (pair? x)) (not (null? x)))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? old (car l)) (cons old (cons new (insertR* new old (cdr l)))))
       (else (cons (car l)
                     (insertR* new old (cdr l))))))
       (else (cons (insertR* new old (car l))
                   (insertR* new old (cdr l)))))))
  
     