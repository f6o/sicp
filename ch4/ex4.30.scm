(load "./ch4-leval.scm")

;; ex 4.30
(define (eval-sequence-cy exps env)
    (if (last-exp? exps)
        (actual-value (first-exp exps) env)
        (begin
            (actual-value (first-exp exps) env)
            (eval-sequence (rest-exps exps) env))))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (if (eq? input 'quit)
        (begin (newline) (display "exit drive-loop") (newline))
        (let ((output (eval input the-global-environment)))
          (announce-output output-prompt)
        (user-print output)
        (driver-loop)))))

(define the-global-environment (setup-environment))
(driver-loop)