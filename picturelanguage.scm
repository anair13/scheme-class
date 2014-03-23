(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))

; excercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (below painter (beside (up-split painter (- n 1))
                             (up-split painter (- n 1))))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

; excercise 2.45
(define (split dir1 dir2)
  (define (dir-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (dir-split painter (- n 1))))
          (dir1 painter (dir2 smaller smaller)))))
  dir-split)

(define right-split (split beside below))
(define up-split (split below beside))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

; excercise 2.46
; vectors
(define (make-vect x y)
  (cons x y))
(define (xcor-vect vector)
  (car vector))
(define (ycor-vect vector)
  (cdr vector))
(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

; excercise 2.47
; frames
(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (car (cdr frame)))
(define (edge2-frame frame)
  (cdr (cdr frame)))

(define (draw-line start end)
  (display 'v)
  (display start)
  (display end))

(define (segments->painter segment-list)
  (lambda (frame)
    (newline)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

; excercise 2.48
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; excercise 2.49
(define (draw-frame f)
  (let* ((T (frame-coord-map f))
         (o (T (make-vect 0 0)))
         (x (T (make-vect 1 0)))
         (y (T (make-vect 0 1)))
         (xy (T (make-vect 1 1)))
         (X (make-segment o x))
         (Y (make-segment o y))
         (XY (make-segment x xy))
         (YX (make-segment y xy)))
    (segments->painter 
     (cons X (cons Y (cons XY (cons YX '())))))))
(define (draw-x f)
  (let* ((T (frame-coord-map f))
         (o (T (make-vect 0 0)))
         (x (T (make-vect 1 0)))
         (y (T (make-vect 0 1)))
         (xy (T (make-vect 1 1)))
         (S1 (make-segment o xy))
         (S2 (make-segment x y)))
    (segments->painter 
     (cons S1 (cons S2 '())))))
(define (draw-diamond f)
  (let* ((T (frame-coord-map f))
         (v1 (T (make-vect 0.5 0)))
         (v2 (T (make-vect 0 0.5)))
         (v3 (T (make-vect 0.5 1)))
         (v4 (T (make-vect 1 0.5)))
         (S1 (make-segment v1 v2))
         (S2 (make-segment v2 v3))
         (S3 (make-segment v3 v4))
         (S4 (make-segment v4 v1)))
    (segments->painter 
     (cons S1 (cons S2 (cons S3 (cons S4 '())))))))
; wave painter
(define default-frame (make-frame (make-vect 0 0) (make-vect 1 0) (make-vect 0 1)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))
(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)   ; new origin
                     (make-vect 1.0 1.0)   ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
((draw-x default-frame) default-frame)
((shrink-to-upper-right (draw-x default-frame)) default-frame)

(define (below painter1 painter2)
  (lambda (frame)
    (((transform-painter painter1 (make-vect 0.0 0.0)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.5)) frame) frame)
    (((transform-painter painter2 (make-vect 0.0 0.5)
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.5)) frame) frame)))
(define (beside painter1 painter2)
  (lambda (frame)
    (((transform-painter painter1 (make-vect 0.0 0.0)
                     (make-vect 0.5 0.0)
                     (make-vect 0.0 1.0)) frame) frame)
    (((transform-painter painter2 (make-vect 0.5 0.0)
                     (make-vect 0.5 0.0)
                     (make-vect 0.0 1.0)) frame) frame)))