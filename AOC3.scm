(define pathA '())
(define pathB '())

(define file "input_AOC3.txt")


(define parse
  (lambda()
    (let ((p (open-input-file file)))
      (let f ((c (read-char p)) (ls '()) (l '()))
	(if (eof-object? c)
	    (begin
	      (close-input-port p)
	       (list (cadr l) (car l)))
	    (let ()
	      (cond ((char-alphabetic? c)
		 (let j ((n (read-char p)) (y '""))
		   (if (char-numeric? n)
		       (begin
			 (set! y (string-append y (string n)))
			 (j (read-char p) y))
		       (if (char=? n #\newline) (f (read-char p) '() (cons (reverse (cons (cons c (string->number y)) ls)) l))
		       (f (read-char p) (cons (cons c (string->number y)) ls) l )))))
		 (else (f (read-char p) ls l)))))))))
		       

(define range
  (case-lambda
    ((i) (range 0 i))
    ((i j)
     (if (< i j)
	 (let iter ((n i) (ls '()))
	   (cond
	    ((> n j) (reverse ls))
	    (else (iter (+ n 1) (cons n ls)))
	    )
	   )
	 ))))

(define (repeat n k)
  (define (iter j ls)
    (do ((i j (- i 1))) ((= i 0) ls) (set! ls (cons k ls))))
  (iter n '()))

(define (move m)
  (case (car m)
    ((#\R)
     (let ((n (cdr m)))
       (map cons (repeat (+ n 1) 0) (range n))))
    ((#\L)
     (let ((n (cdr m)))
       (map cons (repeat (+ n 1) 0) (reverse (range (* -1 n) 0)))))
    ((#\U)
     (let ((n (cdr m)))
       (map cons (range n) (repeat (+ n 1) 0))))
    ((#\D)
     (let ((n (cdr m)))
       (map cons (reverse (range (* -1 n) 0)) (repeat (+ n 1) 0))))))

(define (addmoves moves)
  (do ((move1 moves (cdr moves)) (ls '() (append ls (move (car move1))))) ((null? move1) ls)))

(define (addmoves2 moves)
    (let m ((mv moves) (l '()))
      (if (null? mv) l
	  (begin
	    (let ((toappend (move (car mv))))
	      (display toappend)
	      (m (cdr moves) (append l toappend))))
	  )))

(define findpaths
  (lambda (proc)
    (let ((movesa (car (proc))) (movesb (cadr (proc))))
      (addmoves2 movesa))))
