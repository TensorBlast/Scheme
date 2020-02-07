(define make-accumulator
  (lambda (a)
    (
     let ((amount a))
      (lambda (b)
        (begin
          (set! amount (+ amount b))
          amount
          )
        )
      )
    )
  )


(define (make-monitored f)
  (let ((counter 0))
    (define (mf input)
      (cond
       ((eq? input 'how-many-calls?) counter)
       ((eq? input 'reset-count) (set! counter 0))
       (else
        (begin
          (set! counter (+ 1 counter))
          (f input)))))
    mf
    )
  )


(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
         (begin
           (set! balance (- balance amount))
           balance)
         "Insufficient funds"))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  (define (dispatch p m)
    (if (not (eq? p password)) (lambda (x) "Incorrect password")
        (cond
         ((eq? m 'withdraw) withdraw)
         ((eq? m 'deposit) deposit)
         (else (error "Unknown request -- MAKE ACCOUNT" m)))))
  dispatch)
