(define (accumulate op init seq)
  (if (null? seq) init
      (op (car seq) (accumulate op init (cdr seq)))
      )
  )

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)

    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown Operation -- TABLE" m))))
    dispatch))


(define (make-table2)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (iter-lookup k tab)
        (if (null? k) #f
            (if (null? (cdr k))
                (let ((record (assoc (car k) (cdr tab))))
                  (if record
                      (cdr record) #f))
                (let ((subtable (assoc (car k) (cdr tab))))
                  (if subtable
                      (iter-lookup (cdr k) subtable)
                      #f)))))
      (iter-lookup keys local-table))

    (define (assoc key records)
      (cond ((null? records) #f)
            ((not (pair? records)) #f)
            ((equal? key (caar records)) (car records))
            (else (assoc key (cdr records)))))

    (define (insert! keys value)
      (define (iter-insert k table)
        (if (null? k) #f
            (if (null? (cdr k))
                (let ((record (assoc (car k) (cdr table))))
                  (if record
                      (set-cdr! record value)
                      (set-cdr! table
                                (cons (cons (car k) value)
                                      (cdr table)))))
                (let ((subtable (assoc (car k) (cdr table))))
                  (if subtable
                      (iter-insert (cdr k) subtable)
                      (begin
                        (set-cdr! table
                                  (cons (list (car k)) (cdr table)))
                        (iter-insert k table)))))))
      (iter-insert keys local-table)
      )
    (define (get)
      local-table)
    (define (dispatch m)
      (cond
       ((eq? m 'lookup-proc) lookup)
       ((eq? m 'insert-proc) insert!)
       ((eq? m 'get-table) get)
       ))
    dispatch
    ))

(define table (make-table2))
(define get (table 'lookup-proc))
(define put (table 'insert-proc))
(define disp (table 'get-table))
