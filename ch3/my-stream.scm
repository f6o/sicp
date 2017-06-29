(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define the-null-stream
  '())

(define (stream-enumerate-integer a b)
  (if (> a b)
      the-null-stream
      (cons-stream a
		   (stream-enumerate-integer (+ a 1) b))))
      
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))a

 
