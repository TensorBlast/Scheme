(define (make-queue) (cons '() (cons '() '()))
  )

(define (get-front-ptr q) (car q))

(define (get-rear-ptr q) (cdr q))

(define (set-front-ptr! q item) (set-car! q item))

(define (set-rear-ptr! q item) (set-cdr! q item))

(define (isempty? q) (null? (get-front-ptr q)))

(define (get-first-element q)
  (if (null? (get-front-ptr q)) '()
      (car (get-front-ptr q))
      ))

(define (insert-queue q e)
  (let ((newitem (cons e '())))
    (cond
     ((isempty? q)
      (set-front-ptr! q newitem)
      (set-rear-ptr! q newitem)
      q)
     (else
      (set-cdr! (get-rear-ptr q) newitem)
      (set-rear-ptr! q newitem)
      q)
     )
    )
  )

(define (delete-queue! q)
  (cond ((isempty? q) (error "Cant delete from empty queue" q))
        (else
         (set-front-ptr! q (cdr (get-front-ptr q)))
         q)))

(define (print-queue q)
  (display (get-front-ptr q))
  (newline))
