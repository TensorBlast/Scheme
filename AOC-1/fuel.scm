(define fuel
  (lambda (ls)
    (let ()
      (if (null? ls) 0
	  (+ (fuel (cdr ls)) (- (quotient (car ls) 3) 2))
	  )
      )
    )
  )

(define (sum ls)
  (if (null? ls) 0
      (+ (car ls) (sum (cdr ls)))))

(define (fuel2 initial)
  (let fl ((f initial) (accum 0) (ls '()))
    (if (< f 0) (- accum initial)
	(fl (- (quotient f 3) 2) (+ accum f) (cons f ls)))))

(define getmass
(let ((p (open-input-file "input")))
  (let f ((x (read p)) (ls '()))
    (if (eof-object? x)
	(begin
	  (close-input-port p)
	  (sum ls)
	  )
	(f (read p) (cons (fuel2 x) ls))))))
