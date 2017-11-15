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

