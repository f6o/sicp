;; excercise 4.20

(define (letrec-vars bindings) (map car bindings))
(define (letrec-exps bindings) (map cadr bindings))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec->let exp)
  (cons 'let
        (cons (map (lambda (e) (list e '(quote *unassigned*)))
                   (letrec-vars (letrec-bindings exp)))
              (append
               (map (lambda (c) (cons 'set! c)) (letrec-bindings exp))
               (letrec-body exp)))))

(define test-1 '  (letrec ((even?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    ;; rest of body of f
    ))

(define test-2
  '(letrec ((fact
             (lambda (n)
               (if (= n 1)
                   1
                   (* n (fact (- n 1)))))))
     (fact 10)))
