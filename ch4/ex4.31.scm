(load "./ch4-leval.scm")

;; ex 4.31

;; (define (f a (b lazy) c (d lazy-memo))
;;     ...)

#|
  
* eval calls lambda-parameters if the expression is lambda
* apply calls procedure-parameters if the procedure is compound (i.e. not primitive)
* each argument with lazy or lazy-memo is to be applied with list-of-delayed-args
* Q: What is the implementation difference between lazy and lazy-memo
 
|#

(define the-global-environment (setup-environment))
(driver-loop)
