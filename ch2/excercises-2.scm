;; exercise 2.73 b

(define calctable (make-hash-table))
(define (get calc op) (hash-table-get
                       (hash-table-get calctable calc)
                       op))
(define (put calc op proc)
  (let ((table (hash-table-get calctable calc #f)))
    (display table)
    (if table
        (hash-table-put! table op proc)
        (let ((newtable (make-hash-table)))
          (hash-table-put! newtable op proc)
          (hash-table-put! calctable calc newtable)))))

(define (variable? exp) (symbol? exp))
(define (same-variable? a b) (and (variable? a)
                                  (variable? b)
                                  (eq? a b)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum exps result n)
  (cond
   ((and (null? exps) (null? result)) n)
   ((and (null? exps) (= n 0)) result)
   ((null? exps) (cons n result))
   ((=number? (car exps) 0)
    (make-sum (cdr exps) result n))
   ((number? (car exps))
    (make-sum (cdr exps) result (+ (car exps) n)))
   (else
    (make-sum (cdr exps) (cons (car exps) result) n))))

(define (make-product exps result n)
  (display 'make-product)
  (display exps)
  (display (newline))
  (cond
   ((and (null? exps) (null? result)) n)
   ((and (null? exps) (= n 0)) result)
   ((null? exps) (cons n result))
   ((=number? (car exps) 0) 0)
   ((number? (car exps))
    (make-product (cdr exps) result (* (car exps) n)))
   (else
    (make-product (cdr exps) (cons (car exps) result) n))))

(put 'deriv '+ (lambda (exps var)
                 (make-sum
                  (map (lambda (exp) (deriv exp var))
                       exps) '() 0)))

(put 'deriv '* (lambda (exps var)
                 (make-sum
                  (map
                   (lambda (n exps)
                     (list-set! exps n
                                (deriv (list-ref exps n) var))
                             (make-product exps '() 1))
                   (iota (length exps))
                   (map (lambda (exp)
                          (list-copy exps))
                        exps))
                  '() 0)))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
