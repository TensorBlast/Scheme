
(define (revstring x)
  (list->string  (reverse (string->list x))))

(define percapitameanwealth 386060)

 (define (currencyprint x)
      (let f ((strcurrency (revstring  (number->string x))) (builtstring ""))
        (if (< (string-length strcurrency) 1)
		(revstring (substring builtstring 0 (-(string-length builtstring) 1)))
                (let ((newpart (substring strcurrency (min 3 (string-length strcurrency)) (string-length strcurrency) )))
		(f newpart (string-append builtstring  (substring strcurrency 0 (min 3 (string-length strcurrency))) ","))
		))))
