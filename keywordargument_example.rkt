#lang racket
(define (hello #:name (name "World") #:greeting (greeting "Hello") . args)
  (format "~a ~a, ~a extra args~%" greeting name (length args)))