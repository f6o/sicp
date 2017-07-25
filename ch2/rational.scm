;; exercise 2.1

(define (make-rat n d)
  (if (or (and (negative? n) (negative? d))
          (and (positive? n) (negative? d)))
      (cons (- 0 n) (- 0 d))
      (cons n d)))
