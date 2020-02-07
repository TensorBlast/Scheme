(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq) (accumulate op init (cdr seq)))
      )
  )

(define (enumerate-interval i j)
  (if (> i j) '()
      (cons i (enumerate-interval (+ i 1) j))))

(define a 
 (map (lambda(i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 ( - i 1))))
                 (enumerate-interval 1 5)))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))


(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda(i)
                        (map (lambda(j) (list i j))
                                    (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(define (filter predicate seq)
  (if (null? seq) '()
      (cond
       ((predicate (car seq)) (cons (car seq) (filter predicate (cdr seq))))
       (else
        (filter predicate (cdr seq)))
       )))

(define (square x)
  (* x x))


(define (permutations s)
  (if (null? s) (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s
               )))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))


(define (sum seq)
  (if (null? seq)  0
        (+ (car seq) (sum (cdr seq)))))
