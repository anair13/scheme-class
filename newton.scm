(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? function guess x)
    (< (abs (- (function guess) x)) 0.000001))

(define (update guess x)
    (average guess (/ x guess))
    )

(define (improve update function value guess)
    (if (good-enough? function guess value) 
        guess 
        (improve update function value (update guess value))
        )
    )

(define (msqrt x)
  (improve update square x 1)
  )

(msqrt 100.0)
