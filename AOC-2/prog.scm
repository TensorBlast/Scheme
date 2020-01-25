(define file "input.txt")

(define (sum ls)
  (if (null? ls) 0
      (+ (car ls) (sum (cdr ls)))))

(define txt
  (lambda ()
    (let ((p (open-input-file file)))
      (let f ((c (read-char  p)) (ls '()) (y '""))
	(if (eof-object? c)
	    (begin
	      (close-input-port p)
	      (reverse ls)) (
	    let () (
		    if (char-numeric? c)
			 (begin
			   (set! y (string-append y (string c)))
			   (f (read-char p) ls y))
			 (f (read-char p) (cons y ls) '"")
			 )))))))

(define txt2
  (lambda ()
    (let ((p (open-input-file file)))
      (let f ((c (read-char  p)) (ls '()) (y '""))
	(if (eof-object? c)
	    (begin
	      (close-input-port p)
	      y)
	    (f (read-char p) (cons c ls) (string-append y (string c))))))))

(define (listproc lsr)
  (letrec ((setls (lambda (i ls x)
		    (cond
		     ((null? ls) '())
		   ((= i 0) (set-car! ls x))
		   (else (setls (- i 1) (cdr ls) x)))))
	   (getls (lambda (i ls)
		    (cond
		     ((null? ls) '())
		     ((= i 0) (car ls))
		     (else (getls (- i 1) (cdr ls))))))
	   (process (lambda ( i ls)
		      (if (< i (length ls))
		      (begin 
			(cond
			 ((eqv? (getls i  ls) 99) ls)
			 ((eqv? (getls i ls) 1) (setls (getls (+ i 3) ls) ls (+ (getls (getls (+ i 1) ls) ls) (getls (getls (+ i 2) ls) ls))))
			 ((eqv? (getls i ls) 2) (setls (getls (+ i 3) ls) ls (* (getls (getls (+ i 1) ls) ls) (getls (getls (+ i 2) ls) ls))))
			 (else '()))
			(process (+ i 4) ls)))))
	   (finder (lambda (ls)
		     (let v ((noun 0) (verb 0) (lst ls))
		       (setls 1 lst noun)
		       (setls 2 lst verb)
		       (process 0 lst)
		       ;(display (getls 0 lst))
		       ;(newline)
			 (if (= (getls 0 lst) 19690720)  (cons noun (cons verb '()))
			     (if (< verb 99) (v noun (+ 1 verb) (map (lambda (x) (string->number x)) (txt)))
				 (if (< noun 99) (v (+ 1 noun) 0 (map (lambda (x) (string->number x)) (txt))))))))))
    (begin
      (define lst (map (lambda (x) (string->number x)) lsr))
      ;(setls 1 lst 12)
      ;(setls 2 lst 2)
      ;(process 0 lst)
      ;lst
      (finder lst)
      )))
