(define the-global-environment (setup-environment))
(eval '(define (f x) (+ x 1)) the-global-environment)
(define result (eval '(f 10) the-global-environment))
(if (not (= result 10))
    (error "NG"))




