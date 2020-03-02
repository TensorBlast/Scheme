(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
	    (define (serialized-p . args)
	      (mutex 'acquire)
	      (let ((val (apply p args)))
		(mutex 'release)
		val))
	    serialized-p)))

(define (make-mutex)
  (let ((cell (list #f)))
    (define (the-mutex m)
      (cond
       ((eq? m 'acquire)
	(if (test-and-set! cell) (the-mutex 'acquire)))
       ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell #f))

(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin
	(set-car! cell #t)
	#f)))


;; Serialized Exchange Ex 3.48

(define (make-counter initial)
  (define (counter)
    (set! initial (+ 1 initial))
    initial)
  ((make-serializer) counter))

(define get-id (make-counter 0))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
	(begin
	  (set! balance (- balance amount))
	  balance)
	"Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((balance-serializer (make-serializer)) (id (get-id)))
    (define (dispatch m)
      (cond
       ((eq? m 'withdraw) (balance-serializer withdraw))
       ((eq? m 'deposit) (balance-serializer deposit))
       ((eq? m 'balance) balance)
       ((eq? m 'serializer) balance-serializer)
       ((eq? m 'id) id)
       (else (error "Unknown operation -- MAKE-ACCOUNT" m))))
    dispatch)
)

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance) (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((s1 (account1 'serializer)) (s2 (account2 'serializer)))
    ((if (< (account1 'id) (account2 'id))
	 (s2 (s1 exchange))
	 (s1 (s2 exchage)))
     account1 account2)))
