(define (stream-ref a b)
  (if (= b 0) (stream-car a)
      (stream-ref (stream-cdr a) (- b 1))))

(define (stream-map proc s)
  (if (stream-null? s) the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s) 'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (stream-null? s)
  (null? (stream-car s)))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car stream)
  (car stream))

(define (stream-cdr stream)
  (force (cdr stream)))


(define (stream-enumerate-interval low high)
  (if (> low high) the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ 1 low) high))))

(define the-empty-stream '())

(define (stream-filter pred stream)
  (cond
   ((stream-null? stream) the-empty-stream)
   ((pred (stream-car stream)) (cons-stream (stream-car stream) (stream-filter pred (stream-cdr stream))))
   (else (stream-filter pred (stream-cdr stream)))))

;; IMPLEMENTING DELAY AND FORCE

(define (force delayed-object)
  (delayed-object)
  )

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

(define-syntax delay
  (syntax-rules ()
    ((delay expr)
     (lambda ()
       expr))))


;;

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (ones)
  (cons-stream 1 ones))


(define (divisible x y)
  ( = (remainder x y) 0))

(define no-sevens
  (stream-filter (lambda (x) (not (divisible x 7))) (integers-starting-from 1)))
