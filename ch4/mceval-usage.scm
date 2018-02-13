(define true #t)
(define false #f)

(set!
 primitive-procedures
 (append primitive-procedures
	 (list
	  (list '+ +)
	  (list '- -)
	  (list '= =)
	  (list '> >))))
(define the-global-environment (setup-environment))

;;(display (eval '(define (f x) (+ 1 x)) the-global-environment))
(display (eval '((lambda (x) (+ 1 x)) 10) the-global-environment))

