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

(put 'deriv '+ (lambda (exps var)
                 (make-sum
                  (map (lambda (exp) (deriv exp var)) exps))))

;; (ABC)' = A'BC + AB'C + ABC'
(put 'deriv '* (lambda (exps var)
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
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

;; exercise 2.73 c
(define (make-exponential base exponential)
  (cond ((=number? base 1) 1)
        ((=number? exponential 0) 1)
        ((and (number? base) (number? exponential))
         (fold * 1 (make-list exponential base)))
        (else (list '** base exponential))))

(define (base exps) (car exps))
(define (exponential exps) (cadr exps))

(put 'deriv '** (lambda (exps var)
                  (if (= (length exps) 2)
                      (make-product
                       (list (exponential exps)
                             (make-exponential
                              (base exps)
                              (make-sum
                               (list (exponential exps) -1)))))
                      (error "** should be two arguments."))))

(define (numerator exps) (car exps))
(define (denominators exps) (cdr exps))
(define (make-quotient exps)
  (list '/
        (numerator exps)
        (make-product (denominators exps))))

(define (logarithm exps) (car exps))

(put 'deriv 'log (lambda (exps var)
                   (if (= (length exps) 1)
                       (make-quotient (cons
                                       (deriv (logarithm exps) var)
                                       exps))
                       (error "log should be one argument"))))

;; TODO
(put 'deriv '/ (lambda (exps var)
                 (error "not implemented")))
