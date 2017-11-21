;; run gosh -l./my-evaluator-3.scm driver-loop.test.scm
(define the-global-environment (setup-environment))
(define (eval-assert test expected expr message)
  (let ((actual (eval expr the-global-environment)))
    (if (test actual expected)
	(begin (display "OK") (newline))
	(error #"FAILS: ~|message|"))))
(eval '(define (f x) (+ x 1)) the-global-environment)
(eval '(define a (f 1)) the-global-environment)
(eval '(define b (+ a 1)) the-global-environment)

(eval-assert = 11 '(f 10) "function call")
(eval-assert = 2 'a "defining a global value")
(eval-assert = 3 'b "defining a global value using another var")

(eval '(define a 10) the-global-environment)
(eval-assert = 10 'a "redefining a global value")

(eval '(define (g x)
	 (define u 90)
	 (define v 900)
	 (+ u v x))
      the-global-environment)
(eval-assert = 999 '(g 9) "inner definition")

(eval '(define (h x)
	 (define (even? n)
	   (if (= n 0) true (odd? (- n 1))))
	 (define (odd? n)
	   (if (= n 1) false (even? (- n 1))))
	 (even? x))
      the-global-environment)
(eval-assert = #t '(h 10) "inner definition 2")
