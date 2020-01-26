(load "./ch4-leval.scm")

;; ex 4.31

;; (define (f a (b lazy) c (d lazy-memo))
;;     ...)

;;
;; eval calls lambda-parameters if the expression is lambda
;; apply calls procedure-parameters if the procedure is compound (i.e. not primitive)
;;

(define the-global-environment (setup-environment))
(driver-loop)
