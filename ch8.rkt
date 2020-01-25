#lang racket

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define equal?
  (lambda (a b)
          (cond ((and (atom? a) (atom? b))
                 (eq? a b))
                ((or (atom? a) (atom? b)) #f)
                (else (eqlist? a b))
                )
          )
  )

(define eqlist?
  (lambda (l1 l2)
    (cond ((and (null? l1) (null? l2)) #t)
          ((or (null? l1) (null? l2) #f))
          (else (and (equal? (car l1) (car l2))
                     (eqlist? (cdr l1) (cdr l2))))
          )
    )
  )

(define member?
  (lambda (a lat)
    (cond ((null? lat) #f)
          (else (or (equal? a (car lat)) (member? a (cdr lat))))
          )
    )
  )

(define set?
  (lambda (lat)
          (cond ((null? lat) #t)
                ((member? (car lat) (cdr lat)) #f)
                (else (set? (cdr lat)))
                )
          )
  )

(define subset?
  (lambda (s1 s2)
    (cond ((null? s1) #t)
          (else (and (member? (car s1) s2) (subset? (cdr s1) s2))))
    )
  )

(define intersect?
  (lambda (s1 s2)
    (cond ((null? s1) #f)
          (else (or (member? (car s1) s2) (intersect? (cdr s1) s2))))
  )
  )

(define intersect
  (lambda (s1 s2)
    (cond ((null? s1) (quote ()))
           ((member? (car s1) s2) (cons (car s1) (intersect (cdr s1) s2)))
           (else (intersect (cdr s1) s2))
           )
    )
  )

(define union
  (lambda (s1 s2)
    (cond ((null? s1) s2)
          ((member? (car s1) s2) (union (cdr s1) s2))
          (else (cons (car s1) (union (cdr s1) s2)))
          )
    )
  )

(define intersectall
  (lambda (l-set)
    (cond ((null? (cdr l-set)) (car l-set))
          (else (intersect (car l-set) (intersectall (cdr l-set))))
          )
    )
  )

(define first
  (lambda (x)
    (car x)))

(define second
  (lambda (x)
    (car (cdr x))))
(define eqpair?
  (lambda (p1 p2)
    (and (eq? (first p1) (first p2)) (eq? (second p1) (second p2)))))

(define a-member?
  (lambda (p l-p)
    (cond ((null? l-p) #f)
          (else (or (eqpair? p (car l-p)) (a-member? p (cdr l-p))))
          )
    )
  )

(define a-rel?
  (lambda (rel)
    (cond ((null? rel) #t)
          ((a-member? (car rel) (cdr rel)) #f)
          (else (a-rel? (cdr rel)))
          )))


(define rember-f
  (lambda (test? a l)
    (cond ((null? l) (quote ()))
          ((test? a (car l)) (cdr l))
          (else (cons (car l) (rember-f test? (cdr l))))
          )
    )
  )


(define even1?
  (lambda (n)
    (= (* (/ n 2) 2) n)
    )
  )

(define evens-only-col
  (lambda (l col)
    (cond ((null? l) (col (quote ()) 1 0))
          ((atom? (car l))
           (cond
             ((even? (car l))
              (evens-only-col (cdr l)
                              (lambda (newl p s)
                                (col (cons (car l) newl) (* (car l) p) s))))
             (else
              (evens-only-col (cdr l)
                              (lambda (newl p s)
                                (col newl p (+ (car l) s)))))))
          (else
           (evens-only-col (car l)
             (lambda (al ap as)
               (evens-only-col (cdr l)
                  (lambda (bl bp bs)
                    (col (cons al bl) (* ap bp) (+ as bs))))))))
    )
  )

(define multiinsert-col
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col (quote ()) 0 0))
      ((eq? oldL (car lat))
       (multiinsert-col new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons new (cons oldL newlat)) (+ 1 L) R))))
      ((eq? oldR (car lat))
       (multiinsert-col new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons oldR (cons new newlat)) L (+ 1 R)))))
      (else
       (multiinsert-col new oldL oldR (cdr lat)
          (lambda (newlat L R)
            (col (cons (car lat) newlat) L R)))))
    )
  )