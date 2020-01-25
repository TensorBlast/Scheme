(define nil (quote ()))

(define (filter predicate sequence)
  (if (null? sequence) nil
      (if (predicate (car sequence))
	  (cons (car sequence) (filter predicate (cdr sequence)))
	  (filter predicate (cdr sequence))
	  )
      )
  )

(define (enumerate-interval low high)
  (if (> low high) nil
      (cons low (enumerate-interval (add1 low) high))))


(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence))
	  )
      )
  )


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter (lambda (positions) (safe? k positions))
		(flatmap
		 (lambda (rest-of-queens)
		   (map (lambda (new-row)
			  (adjoin-position new-row k rest-of-queens))
			(enumerate-interval 1 board-size)))
		 (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board nil)

(define (snoc a b)
  (reverse (cons a (reverse b))))

(define (adjoin-position new-row k rest-of-queens)
  (snoc new-row rest-of-queens))

(define (safe? k positions)
  (define (next-col-safe? colnum offset)
    (if (= colnum 0) #t
    (let ((this-row (list-ref positions (sub1 k))) (prevrow (list-ref positions (sub1 colnum))))
      (if (or
	   (= this-row prevrow)
	   (= this-row (- prevrow offset))
	   (= this-row (+ prevrow offset))) #f
	   (next-col-safe? (- colnum 1) (+ offset 1))
	   ))))
  (next-col-safe? (- k 1) 1))
      
