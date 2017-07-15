(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define the-null-stream
  '())

(define (stream-null? s)
  (null? s))

(define (stream-enumerate-integer a b)
  (if (> a b)
      the-null-stream
      (cons-stream a
		   (stream-enumerate-integer (+ a 1) b))))
      
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-filter pred? s)
  (cond ((stream-null? s)
	 the-null-stream)
	((pred? (stream-car s))
	 (cons-stream (stream-car s)
		      (stream-filter pred? (stream-cdr s))))
	(else (stream-filter pred? (stream-cdr s)))))

      
