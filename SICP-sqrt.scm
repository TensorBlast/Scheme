(define (sqiter guess delta x)
  (if (goodenough? guess delta x) guess
      (let* ((nextguess (improve guess x)) (dlta (abs (- guess nextguess))))
	(sqiter nextguess dlta x))))

(define (goodenough? guess delta x)
  (< (/ delta guess) 0.0001))

(define (improve guess x)
  (avg guess (/ x guess)))

(define (avg x y)
  (/ (+ x y) 2))

(define (sqrt1 x)
  (sqiter 1 1 x))
