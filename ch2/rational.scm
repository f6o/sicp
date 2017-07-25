;; exercise 2.1

(define (make-rat n d)
  (if (or (and (negative? n) (negative? d))
          (and (positive? n) (negative? d)))
      (cons (- 0 n) (- 0 d))
      (cons n d)))

;; exercise 2.2

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment sx sy ex ey)
  (cons (make-point sx sy)
        (make-point ex ey)))

(define (start-segment seg) (car seg))
(define (end-segment seg) (cdr seg))

(define (add-point p1 p2)
  (make-point (+ (x-point p1)
                 (x-point p2))
              (+ (y-point p1)
                 (y-point p2))))

(define (scale-point p n)
  (make-point (* (x-point p) n)
              (* (y-point p) n)))

(define (midpoint-segment seg)
  (scale-point (add-point (start-segment seg)
                          (end-segment seg))
               0.5))

;; excercise 2.3

;; -- 長方形の対角に位置する点の cons セル

;; (define (make-rectangle px py qx qy) (cons (make-point px py) (make-point qx qy)))
;; (define (origin-rectangle r) (car r))
;; (define (diagonal-rectangle r) (cdr r))

;; -- 対角に位置する点の座標のリスト
(define (make-rectangle px py qx qy) (list px py qx qy))
(define (origin-rectangle r) (cons (car r) (cadr r)))
(define (diagonal-rectangle r) (cons (caddr r) (cadddr r)))

;; -- 以下の手続きには変更を加えないまま

(define (x-difference p1 p2)
  (- (x-point p1) (x-point p2)))

(define (y-difference p1 p2)
  (- (y-point p1) (y-point p2)))

(define (width-rectangle r)
  (abs (x-difference (diagonal-rectangle r)
                     (origin-rectangle r))))

(define (height-rectangle r)
  (abs (y-difference (diagonal-rectangle r)
                     (origin-rectangle r))))

(define (perimeter-rectangle r)
  (* 2 (+ (width-rectangle r)
          (height-rectangle r))))

(define (area-rectangle r)
  (* (width-rectangle r) (height-rectangle r)))

