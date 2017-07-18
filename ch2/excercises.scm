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

;; 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x)
  (cadr x))

(define (exponent x)
  (caddr x))

(define (make-exponentiation a n)
  (cond ((= n 0) 1)
        ((= n 1) a)
        (#t (list '** a n))))

;; exercise 2.57

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
  (if (= (length (cddr s)) 1)
      (caddr s)
      (cons '+ (cddr s))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
  (if (= (length (cddr p)) 1)
      (caddr p)
      (cons '* (cddr p))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (num-or-var? exp)
  (or (number? exp) (variable? exp)))

(define (make-sum first rest)
  (cond ((=number? first 0) rest)
        ((=number? rest 0) first)
        ((and (number? first) (number? rest)) (+ first rest))
        ((and (num-or-var? first) (num-or-var? rest))
         (list '+ first rest))
        ((and (eq? (car first) '+) (eq? (car rest) '+))
         (append first (cdr rest)))
        ((eq? (car first) '+)
         (append first (list rest)))
        ((eq? (car rest) '+)
         (append (list '+ first) (cdr rest)))
        (else
         (list '+ first rest))))

(define (make-product first rest)
  (cond ((or (=number? first 0) (=number? rest 0)) 0)
        ((=number? first 1) rest)
        ((=number? rest 1) first)
        ((and (number? first) (number? rest)) (* first rest))
        ((and (num-or-var? first) (num-or-var? rest))
         (list '* first rest))
        ((and (eq? (car first) '*) (eq? (car rest) '*))
         (append first (cdr rest)))
        ((eq? (car first) '*)
         (append first (list rest)))
        ((eq? (car rest) '*)
         (append (list '* first) (cdr rest)))
        (else (list '* first rest))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponentiation (base exp)
                                            (- (exponent exp) 1))))
        (else
         (error "unknown expression type -- DERIV" exp))))
