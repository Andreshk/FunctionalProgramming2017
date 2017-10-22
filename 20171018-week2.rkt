#lang racket
; Предефиниране на if с and/or
;(define (if* p x y)
;  (or (and p x) y))
; Проблем: ако x e #f, тогава се връща y...
; (if* #t #f "oops") -> "oops"

; Предефиниране на булевите операции с if
(define (not* x)
  (if x #f #t))

(define (and* x y)
  (if x y #f))

(define (or* x y)
  (if x #t y))

; Ако някъде имплицитната конверсия към bool не е достатъчна
(define (->bool x)
  (if x #t #f))

; Второто Hello, world на функционалното програмиране: Фибоначи
(define (fib n)
  (if (< n 2) n
      (+ (fib (- n 1)) (fib (- n 2)))))

; С проверка на входните данни и връщане на "безсмислена"
; стойност. Все пак е съмнителна практика в контекста на ФП...
(define (fib* n)
  (cond ((or (not (integer? n)) (< n 0)) #f)
        ((< n 2) n)
        (else (+ (fib* (- n 1)) (fib* (- n 2))))))

; Биномен коефициент (с няколко локални дефиниции
(define (nchk n k)
  (define (fact n)
    (if (= n 0) 1 (* n (fact (- n 1)))))
  (define n! (fact n))
  (define k! (fact k))
  (define n-k! (fact (- n k)))
  
  (/ n! (* k! n-k!)))

; Бонимен коефициент, по рекурсивната формула
(define (nchk* n k)
  (cond [(= n k) 1] ; може и (or (= n k) (zero? k))
        [(zero? k) 1]
        [else (+ (nchk* (- n 1) (- k 1))
                 (nchk* (- n 1) k))]))

; Най-голям общ делител
;(define (gcd a b)
;  (cond [(< a b) (gcd b a)]
;        [(= a 0) b]
;        [(= b 0) a]
;        [(= a b) a]
;        [else (gcd (remainder a b) b)]))

; Също НОД, но с инварианта винаги a >= b
(define (gcd a b)
  (cond [(< a b) (gcd b a)]
        [(or (= b 0) (= a b)) a]
        [else (gcd b (remainder a b))]))

; Най-малко общо кратно
(define (lcm a b)
  (/ (* a b) (gcd a b)))
;(define (lcm a b)
;  (* (/ a (gcd a b)) b))

; Брой корени на квадратно уравнение по дадени коефициенти
(define (roots a b c)
  (define D (- (* b b) (* 4 a c)))
  (cond [(and (= a 0) (= b 0)) 0]
        [(= a 0) 1]
        [(< D 0) 0]
        [(= D 0) 1]
        [else 2]))

; Бързо степенуване на целочислена степен,
; с две помощни дефиниции за подобрена четимост на кода.
(define (fast-expt x n)
  (define (sq x) (* x x))
  (define half (quotient n 2))
  (cond [(< n 0) (/ 1 (fast-expt x (- n)))]
        [(= n 0) 1]
        [(even? n) (sq (fast-expt x half))]
        [else (* x (sq (fast-expt x half)))]))





















