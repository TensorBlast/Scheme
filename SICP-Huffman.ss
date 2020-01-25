(define (append list1 list2)
  (if (null? list1)
      list2
      (if (not (pair? list1)) (cons list1 list2)
      (cons (car list1) (append (cdr list1) list2)))))

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (symbol-leaf l)
  (cadr l))

(define (weight-leaf l)
  (caddr l))


(define (leaf? l)
  (eq? 'leaf (car l)))


(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))
	)
  )

(define (symbols tree)
  (if (leaf? tree) (symbol-leaf tree)
     ( caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree)
      (cadddr tree)
  ))

(define (left-branch tree)
  (car tree))

(define (right-branch tree) (cadr tree))

					

(define (choose-branch bit branch)
  (cond
   ((= bit 0) (left-branch branch))
   ((= bit 1) (right-branch branch))
   (else (error "Bad bit -- CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
	(let ((next-branch (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree)
  )

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else
	 (cons (car set) (adjoin-set x (cdr set))))
	))


(define (make-leaf-set pairs)
  (if (null? pairs) '()
   (let ((pair (car pairs)))
     (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
		  (make-code-tree
		   (make-leaf 'B 2)
		   (make-code-tree (make-leaf 'D 1)
				   (make-leaf 'C 1)))))


(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
	      (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-1 symbol branch bits)
    (if (leaf? branch)
	(if (eqv? symbol (symbol-leaf branch)) bits
	    '())
	(let ((left (encode-1 symbol (left-branch branch) (reverse (cons 0 bits))))
	      (right (encode-1 symbol (right-branch branch) (reverse (cons 1 bits)))))
	  (if (null? left) right
	      left))))
  (encode-1 symbol tree '()))
