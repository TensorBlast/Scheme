#lang racket

(define atom?
  (lambda (x)
          (and (not (pair? x)) (not (null? x)))))

;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;      ((and (null? l1) (null? l2)) #t)
;      ((and (null? l1) (atom? (car l2)) #f))
;      ((null? l1) #f)
;      ((and (atom? (car l1)) (null? l2)) #f)
;      ((and (atom? (car l1)) (atom? (car l2)))
;       (and (eq? (car l1) (car l2))
;            (eqlist? (cdr l1) (cdr l2))))
;      ((atom? (car l1)) #f)
;      ((null? l2) #f)
;      ((atom? (car l2)) #f)
;      (else
;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))
;       ))))


;(define eqlist?
;  (lambda (l1 l2)
;    (cond
;      ((and (null? l1) (null? l2)) #t)
;      ((or (null? l1) (null? l2)) #f)
;      ((and (atom? (car l1)) (atom? (car l2)))
;       (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
;      ((or (atom? (car l1)) (atom? (car l2))) #f )
;      (else
;       (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))



(define equal?
  (lambda (s1 s2)
          (cond
            ((and (atom? s1) (atom? s2)) (eq? s1 s2))
            ((or (atom? s1) (atom? s2)) #f)
            (else
             (eqlist? s1 s2)))))

(define eqlist?
  (lambda (l1 l2)
          (cond
            ((and (null? l1) (null? l2)) #t)
            ((or (null? l1) (null? l2)) #f)
            (else
             (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2)))
             ))))