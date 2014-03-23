(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (value card)
  (cond ((equal? (butlast card) 'j) 10)
        ((equal? (butlast card) 'q) 10)
        ((equal? (butlast card) 'k) 10)
        ((equal? (butlast card) 'J) 10)
        ((equal? (butlast card) 'Q) 10)
        ((equal? (butlast card) 'K) 10)
        (else (butlast card))))

(define (card-val card ace-high?)
  (if (or (equal? (butlast card) 'a) (equal? (butlast card) 'A)) (if ace-high? 11 1) (value card)))

(define (suit card)
  (last card))

(define (best-total cards)
  (define (count-aces cards)
    (if (empty? cards) 0
      (if (equal? (value (first cards)) 'a) (+ 1 (count-aces (butfirst cards))) (count-aces (butfirst cards)))))
  (define (total-without-aces cards)
    (if (empty? cards) 0 (+ (total-without-aces (butfirst cards))
      (cond ((equal? (value (first cards)) 'a) 0)
            ((equal? (value (first cards)) 'A) 0)
            (else (value (first cards)))))))
  (define (choose-best-total total aces)
    (if (= 0 aces) total 
       (if (>= 11 (+ total aces))
           (choose-best-total (+ total 11) (- aces 1)) 
           (choose-best-total (+ total 1) (- aces 1)))))
  (choose-best-total (total-without-aces cards) (count-aces cards)))
  
(best-total '(ad 8s))    ; 19
(best-total '(ad 8s 5h)) ; 14
(best-total '(ad as 9h)) ; 21

(define (play-n strategy n)
  (if (= 0 n) 0 (+ (twenty-one strategy) (play-n strategy (- n 1)))))

(define (stop-at-17 hand-so-far dealer-up-card)
  (< (best-total hand-so-far) 17))

(play-n stop-at-17 100)

(define (dealer-sensitive hand-so-far dealer-up-card)
  (or 
   (and (< (best-total hand-so-far) 17) (>= (card-val dealer-up-card #t) 7))
   (and (< (best-total hand-so-far) 12) (<= (card-val dealer-up-card #t) 6))))

(play-n dealer-sensitive 100)

(define (stop-at n)
  (define (stop-at-n hand-so-far dealer-up-card)
    (< (best-total hand-so-far) n))
  stop-at-n)

(play-n (stop-at 17) 100)

(define (valentine hand-so-far dealer-up-card)
  (define (has-heart? cards)
    (if (empty? cards) #f
      (if (equal? (suit (first cards)) 'h) #t (has-heart? (butfirst cards)))))
  (if (has-heart? hand-so-far)
    (< (best-total hand-so-far) 19)
    (< (best-total hand-so-far) 17)))

(define (suit-strategy the-suit yes-strategy no-strategy)
  (define (has-suit? cards)
    (if (empty? cards) #f
      (if (equal? (suit (first cards)) the-suit) #t (has-suit? (butfirst cards)))))
  (define (strategy hand-so-far dealer-up-card)
    (if (has-suit? hand-so-far)
        (yes-strategy hand-so-far dealer-up-card)
        (no-strategy hand-so-far dealer-up-card)))
  strategy)

(play-n valentine 100)
(play-n (suit-strategy 'h (stop-at 19) (stop-at 17)) 100)      

(define (majority s1 s2 s3)
  (define (strategy hand-so-far dealer-up-card)
    (if (equal? (s1 hand-so-far dealer-up-card) (s2 hand-so-far dealer-up-card))
      (s1 hand-so-far dealer-up-card)
      (s3 hand-so-far dealer-up-card)))
  strategy)

(play-n (majority stop-at-17 dealer-sensitive valentine) 100)

(define (reckless strategy)
  (define (reckless-strategy hand-so-far dealer-up-card)
    (if (empty? hand-so-far) #t (strategy (butlast hand-so-far) dealer-up-card)))
  reckless-strategy)

(play-n (reckless (stop-at 17)) 100)