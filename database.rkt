#lang racket

(define first
  (lambda (list)
    (car list)))

(define second
  (lambda (list)
    (car (cdr list))))

(define build
  (lambda (a b)
    (cons a
           (cons b (quote ()) )
           
          )
    )
  )

(define new-entry
  (lambda (keys values)
    (build keys values
           )
    )
  )

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
      (first entry)
      (second entry)
      entry-f)))

(define lookup-in-entry-help
  (lambda (name keys values entry-f)
    (cond
      ((null? keys) (entry-f name))
      ((eq? name (car keys)) (car values))
      (else
       (lookup-in-entry-help
        name
        (cdr keys)
        (cdr values)
        entry-f))
      )
    )
  )

(define extend-table
  (lambda (entry table)
    (cons entry table)
    )
  )

(define lookup-in-table
  (lambda (name table table-f)
    (cond
      ((null? table) (table-f name))
      (else
       (lookup-in-entry name (first table)
                        (lambda (name)
                          (lookup-in-table name
                                           (cdr table) table-f))
                        )
       )
      )
    )
  )

