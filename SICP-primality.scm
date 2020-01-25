(import (chicken random))
 
(define (random n)
  (pseudo-random-integer n))

(define square
  (lambda (x) (* x x)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (+ test-divisor 1)))))


(define (divides? a b)
  (= (remainder b a) 0)
  )

(define (prime? n)
  (eqv? (smallest-divisor n) n))

(define (expmod base exp m)
  (cond
   ((= exp 0) 1)
   ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
   (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test m)
  (define (trial a)
    (= (expmod a m m) a))
  (trial (+ 1 (random (- m 1)))))

(define (fast-prime n times)
  (cond
   ((= times 0) #t)
   ((fermat-test n) (fast-prime n (- times 1)))
   (else #f)))
