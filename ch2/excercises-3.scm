;; exercise 2.73 d

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

(define (make-sum exps)
  (define (make-sum-inner exps result n)
    (cond
     ((and (null? exps) (null? result)) (list n))
     ((and (null? exps) (= n 0)) result)
     ((null? exps) (cons n result))
     ((=number? (car exps) 0)
      (make-sum-inner (cdr exps) result n))
     ((number? (car exps))
      (make-sum-inner (cdr exps) result (+ (car exps) n)))
     (else
      (make-sum-inner (cdr exps) (cons (car exps) result) n))))
  (let ((result (make-sum-inner exps '() 0)))
    (if (> (length result) 1)
        (cons '+ result)
        (car result))))

(define (make-product exps) 
  (define (make-product-inner exps result n)
    (cond
     ((and (null? exps) (null? result)) (list n))
     ((and (null? exps) (= n 1)) result)
     ((null? exps) (cons n result))
     ((=number? (car exps) 0) (list 0))
     ((=number? (car exps) 1)
      (make-product-inner (cdr exps) result n))
     ((number? (car exps))
      (make-product-inner (cdr exps) result (* (car exps) n)))
     (else
      (make-product-inner (cdr exps) (cons (car exps) result) n))))
  (let ((result
         (make-product-inner exps '() 1)))
    (if (> (length result) 1)
        (cons '* result)
        (car result))))

(put '+ 'deriv (lambda (exps var)
                 (make-sum
                  (map (lambda (exp) (deriv exp var)) exps))))

(put '* 'deriv (lambda (exps var)
                 (make-sum
                  (map (lambda (n exps)
                         (list-set! exps n
                                    (deriv (list-ref exps n) var))
                         (make-product exps))
                       (iota (length exps))
                       (map (lambda (exp)
                              (list-copy exps))
                            exps)))))


(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ;; EXCHANGED operator AND 'deriv
        (else ((get (operator exp) 'deriv)
               (operands exp) var))))

