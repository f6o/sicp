;; exercise 4.3

(define apply-in-underlying-scheme apply)

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define functable (make-hash-table))
(define (get type) (hash-table-get functable type #f))
(define (put type item) (hash-table-put! functable type item))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((get (car exp)) ((get (car exp)) exp env))	
	((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; used in begin and apply
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;; excercise 4.4: and & or
(define (expressions exp) (cdr exp))

(define (eval-and-expressions exps env)
  (cond ((last-exp? exps)
	 (eval (first-exp exps) env))
	(else (if (eval (first-exp exps) env)
		  (eval-expressions (rest-exps exps) env)
		  false))))

(define (eval-and exp env)
  (cond ((pair? (expressions exp))
	 (eval-and-expressions (expressions exp) env))
	(else #t)))

(put 'and eval-and)

(define (eval-or-expressions exps env)
  (cond ((last-exp? exps)
	 (eval (first-exp exps) env))
	(else
	 (let ((val (eval (first-exp exps) env)))
	   (if val
	       val
	       (eval-expressions (rest-exps exps) env))))))

(define (eval-or exp env)
  (cond ((pair? (expressions exp))
	 (eval-or-expressions (expressions exp) env))
	(else #f)))

(put 'or eval-or)

;; and & or as derived form

(define (expand-and exps)
  (if (last-exp? exps)
      (car exps)
      (let ((first (car exps))
	    (rest (cdr exps)))
	(make-if first
		 (expand-and rest)
		 'false))))

(define (and->if exp)
  (expand-and (expressions exp)))

(put 'and2 (lambda (exp env)
	     (eval (and->if exp) env)))

(define (expand-or exps)
  (if (last-exp? exps)
      (car exps)
      (let ((first (car exps))
	    (rest (cdr exps)))
	(make-if first
		 first
		 (expand-and rest)))))

(define (or->if exps)
  (expand-or (expressions exps)))

(put 'or2 (lambda (exp env)
	    (eval (or->if exp) env)))

;; self-evaluating

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

;; -- helper procedure

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

;; quote

(put 'quote
     (lambda (exp env) (text-of-quotation exp)))

(define (text-of-quotation exp) (cadr exp))

;; variable

(define (variable? exp) (symbol? exp))

;; set!

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)
(put 'set! eval-assignment)

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; define

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)
(put 'define eval-definition)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;; lambda

(put 'lambda
     (lambda (exp env)
       (make-procedure (lambda-parameters exp) (lambda-body exp)
		       env)))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(put 'if eval-if)

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin

(put 'begin
     (lambda (exp env)
       (eval-sequence (begin-actions exp) env)))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;; apply procedure

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;; cond using if

(put 'cond
     (lambda (exp env)
       (eval (cond->if exp) env)))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

;; excercise 4.5

(define (cond-extend-clause? clause)
  (eq? (cond-extend-tag clause) '=>))

(define (cond-extend-tag clause) (cadr clause))
(define (cond-extend-proc clause) (caddr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
	(cond ((cond-else-clause? first)
	       (if (null? rest)
		   (sequence->exp (cond-actions first))
		   (error "ELSE clause isn't last -- COND->IF"
			  clauses)))
	      ((cond-extend-clause? first)
	       (let ((val (cond-predicate first)))
		 (make-if val
			  (list (cond-extend-proc first)
				val)
			  (expand-clauses rest))))
	      (else (make-if (cond-predicate first)
			     (sequence->exp (cond-actions first))
			     (expand-clauses rest)))))))

;; exercise 4.6 let

(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (cons (make-lambda (vars-in-bindings bindings)
		       (let-body exp))
	  (exps-in-bindings bindings))))

(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (variable? (cadr exp))
       (pair? (cdddr exp))))

(define (named-let->let exp)
  (let ((bindings (named-let-bindings exp)))
    (list 'let
	  '() 
	  (list 'define
		(named-let-name exp)
		(make-lambda (vars-in-bindings bindings)
			     (named-let-body exp)))
	  (cons (named-let-name exp)
		(exps-in-bindings bindings)))))

(define (named-let-name exp) (cadr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-body exp) (cdddr exp))

;; TODO: why f can't be found?
;;
;; (let ((f (lambda (n)
;; 	   (if (= n 1)
;; 	       1
;; 	       (* n (f (- n 1)))))))
;;   (f 5))

;; exercise 4.8 named-let
;;
;; example 1:
;; 
(let f ((n 5))
  (if (= n 1)
      1
      (* n (f (- n 1)))))
;;
;; (let ()
;;   (define f (lambda (n)
;; 	      (if (= n 1)
;; 		  1
;; 		  (* n (f (- n 1))))))
;;   (f 5))
;;
;; example 2
;;
;; (define (fib n)
;;   (let fib-iter ((a 1)
;; 		 (b 0)
;; 		 (count n))
;;     (if (= count 0)
;; 	b
;; 	(fib-iter (+ a b) a (- count 1)))))
;;
;; will be translated into:
;;
;; (define (fib n)
;;   (let ()
;;     (define fib-iter
;;       (lambda (a b count)
;; 	(if (= count 0)
;; 	    b
;; 	    (fib-iter (+ a b) a (- count 1)))))
;;     (fib-iter 1 0 n)))

(define (let-bindings exp)
  (cadr exp))

(define (vars-in-bindings bindings)
  (map car bindings))

(define (exps-in-bindings bindings)
  (map cadr bindings))

(define (let-body exp)
  (cddr exp))

(put 'let (lambda (exp env)
	    (eval (let->combination exp) env)))

;; exercise 4.7 let*

(define (make-let bindings body)
  (cons 'let
	(cons
	 (list bindings)
	 body)))

;;
;; (let* ((x 3)
;;        (y (+ x 2))
;;        (z (+ x y 5)))
;;   (* x z))
;;
;; the let* expression should be translated into:
;;
;; (let ((x 3))
;;   (let ((y (+ x 2)))
;;     (let ((z (+ x y 5)))
;;       (* x z))))

;; TODO: I think it's un-natural to wrap make-let with list.
(define (let*->nested-lets exp)
  (define (inner bindings)
    (if (null? bindings)
	(let-body exp)
	(list (make-let (car bindings)
			(inner (cdr bindings))))))
  (car (inner (let-bindings exp))))

(put 'let* (lambda (exp env)
	     (eval (let*->nested-lets exp) env)))

;; ---

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
	(list 'list list)
	(list '+ +)
	(list '* *)))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;; our own REPL!

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))
