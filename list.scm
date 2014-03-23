(define (getelement l n)
	(cond ((= n 0) (car l))
		(else (getelement (cdr l) (- n 1)))))

(getelement (cons 1 (cons 2 (cons 3 (cons 4 '())))) 3)