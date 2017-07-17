;; exercise 2.53

;; '(a b c)
(list 'a 'b 'c)

;; '((george))
(list (list 'george))

;; '((y1 y2))
(cdr '((x1 x2) (y1 y2)))

;; '(y1 y2)
(cadr '((x1 x2) (y1 y2)))

;; #f
(pair? (car '(a short list)))

;; #f
(memq 'red '((red shoes) (blue socks)))

;; '(red shoes blue socks)
(memq 'red '(red shoes blue socks))

;; exercise 2.54

(define (equal? a b)
  (if (and (pair? a) (pair? b))
      (if (eq? (car a) (car b))
          (equal? (cdr a) (cdr b))
            #f)
      (and (null? a) (null? b))))

(equal? '(this is a list) '(this is (a test) list))

;; exercise 2.55

(car ''abracadabra)

;; (car (quote 'abracadabra)) と展開されるから
