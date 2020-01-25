#lang racket


(define avg
  (lambda (l)
    (letrec ([accumulate (lambda (x)
                           (if (null? x) 0
                               (+ (car x) (accumulate (cdr x)))))]
             [len length])
      (/ (accumulate l) (len l))
      )
    )
  )