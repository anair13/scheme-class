#lang racket

; implements tic-tac-toe, through a number game
; connected by a 3x3 magic square, one such square:
; 492
; 357
; 816

; combinations algorithm
; x is length of elements
; n in number of elements to choose
(define (choose elements n)
  (define (combinations elements x n)
    (if (= x n)
        (list elements) ; only one way to pick nCn
        (if (or (= n 0) (< x n))
            '(())
            (append (combinations (cdr elements) (- x 1) n) ; without first element
                    (map (lambda (l) (append (list (car elements)) l)) ; with first element
                         (combinations (cdr elements) (- x 1) (- n 1)))))))
  (combinations elements (length elements) n))

(define (sum elements)
  (foldl + 0 elements))
(define (in? elements x)
  (define (in elements n x)
    (if (= n 0) 
        #f
        (if (= x (car elements)) 
            #t
            (in (cdr elements) (- n 1) x))))
  (in elements (length elements) x))

; win condition: 3 elements that add up to 15
(define (win? elements)
  (in? (map sum (choose elements 3)) 15))

; human player just reads input
(define (human-player me them)
  (read))

(define (counter)
  (define x 0)
  (lambda () (set! x (+ x 1)) x))

(define c1 (counter))
(define c2 (counter))
; you and them are lists of moves
; minimax search for best move
; returns (score move)
(define (best-move-1 you them)
  (c1)
  (if (win? them) (list -1 0)
      (if (= (length (valid-moves you them)) 0) (list 0 0) ;draw
          (argmax car 
                  (map (lambda (m) (list (- (car (best-move-1 them (cons m you)))) m)) 
                       (valid-moves you them))))))

; minimax with alpha-beta pruning
; returns (score move)
(define (best-move-2 you them)
  ; returns (alpha beta move)
  (define (best-move-pruned you them alpha beta)
    (c2)
    (if (win? them) (list -1 1 0)
        (let ((moves (valid-moves you them)))
          (if (= (length moves) 0) (list 0 2 0)
              (foldl (lambda (m c) ; c: (alpha, beta, m)
                       (if (>= (car c) (cadr c)) c
                           (let ((move (best-move-pruned them (cons m you) (car c) (cadr c))))
                             (if (> (- (car move)) (car c))
                                 (list (- (car move)) (cadr move) m)
                                 c)
                             )))
                     (list alpha beta 0)
                     moves)))))
  (let ((m (best-move-pruned you them -2 2)))
    (list (car m) (caddr m))))

(define (machine-player me them)
  (let ((m (best-move-2 me them)))
    (display (cadr m)) (newline)
    (cadr m)))

(define (valid-move x moves1 moves2) 
  (not (or (in? moves1 x) (in? moves2 x))))
(define (valid-moves moves1 moves2)
  (filter (lambda (x) (valid-move x moves1 moves2)) (list 1 2 3 4 5 6 7 8 9)))

; controls gameplay
; returns 0 for draw, 1 for player1 win, 2 for player2 win
(define (game player1 player2)
  ; player1 is player to make current move
  ; moves1 <-> player1, moves2 <-> player2
  ; player who started is player[turn]
  (define (game player1 moves1 player2 moves2 turn)
    (display "----PLAYER") (display turn) (display "----") (newline)
    (display "you:") (display moves1) (newline)
    (display "them:") (display moves2) (newline)
    (display "valid_moves:") (display (valid-moves moves1 moves2)) (newline)
    (display turn) (display ": ")
    (if (= (length (valid-moves moves1 moves2)) 0) 0 ; draw
        (let ((pick (player1 moves1 moves2)))
          (if (valid-move pick moves1 moves2)
              (let ((move (cons pick moves1)))
                (if (win? move) turn ; game ends and winner is returned
                    (game player2 moves2 player1 move (- 3 turn)))) ; next move
              (game player1 moves1 player2 moves2 turn))))) ; invalid move, redo
  (game player1 '() player2 '() 1))

;(best-move-1 '() '())
;(c1)
;(best-move-2 '() '())
;(c2) ; shows that best-move-2 prunes

;run the game
(let ((result (game human-player machine-player))) 
  (cond ((= result 0) (display "Draw!"))
    ((= result 1) (display "Player 1 Wins!"))
    ((= result 2) (display "Player 2 Wins!"))))