(define (accumulate op initial sequence)
  (if (null? sequence) initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (map proc sequence)
  (if (null? sequence) '()
      (cons (proc (car sequence)) (map proc (cdr sequence)))))


(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(+ this-coeff (* x higher-terms)))
	      0
	      coefficient-sequence))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (n) (dot-product v n))  m))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define transpose
  (lambda (mat)
    (accumulate-n cons '() mat)))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rows) (matrix-*-vector cols rows))
	 m)))

(define (matrix-*-matrix2 m n) 
   (let ((n-cols (transpose n))) 
     (map (lambda (m-row) 
            (map (lambda (n-col)  
                   (dot-product m-row n-col))  
                 n-cols)) 
          m))) 
