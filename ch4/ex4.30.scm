(load "./ch4-leval.scm")

;; ex 4.30
(define (eval-sequence-cy exps env)
    (if (last-exp? exps)
        (actual-value (first-exp exps) env)
        (begin
            (actual-value (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define the-global-environment (setup-environment))
(driver-loop)