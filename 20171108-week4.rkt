#lang racket
(define (1+ x) (+ x 1))
(define (id x) x)

; accumulate - изчислява (1+(2+(3+(4+0)))
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

; filter-accumulate - първо филтрира числата от импровизираната редица,
; преди да смята term за тях - напр. (1+(3+0))
(define (filter-accum op nv a b term next p?)
  (cond [(> a b) nv]
        [(p? a) (op (term a)
                    (filter-accum op nv (next a) b term next p?))]
        [else       (filter-accum op nv (next a) b term next p?)]))

; итеративна версия на accumulate - изчислява ((((0+1)+2)+3)+4)
; може да върне различен резултат от accumulate при некомутативна операция
(define (accumulate-i op nv a b term next)
  (define (helper res i)
    (if (> i b)
        res
        (helper (op res (term i)) (next i)))) ;((((0+1)+2)+3)+4)
       ;(helper (op (term i) res) (next i)))) ;(4+(3+(2+(1+0))))
       ; ако обърнем аргументите на op, получаваме може би друг резултат
  (helper nv a))

; и filter-accumulate има итеративен вариант
(define (filter-accum-i op nv a b term next p?)
  (define (helper res i)
    (cond [(> i b) res]
          [(p? i) (helper (op res (term i)) (next i))]
          [else   (helper     res           (next i))])) ;не променяме резултата
  (helper nv a))

; Зад.1
(define (!! n)
  (define start (if (odd? n) 1 2))
  (accumulate * 1 start n id (lambda (i) (+ i 2))))

; Зад.2
;(define (nchk n k)
;  (/ (accumulate * 1 (+ n (- k) 1) n id 1+)
;     (accumulate * 1 1             k id 1+)))

;(define (fact n)
;  (accumulate * 1 1 n id 1+))

;(define (nchk n k)
;  (/ (fact n)
;     (* (fact k) (fact (- n k)))))

; Зад.3: досещаме се, че можем да изразим биномния коефициент
; по-лесно ако го представим като n(n-1)...(n-k+1) / 1.2...k
(define (nchk n k)
  (accumulate * 1 1 k
              (lambda (i) (/ (+ n 1 (- i)) i))
              1+))

; Зад.4
;(define (2^ n)
;  (accumulate * 1 1 n (lambda (i) 2) 1+))

(define (2^ n)
  (accumulate + 0 0 n (lambda (i) (nchk n i)) 1+))

; Зад.5 - решение с модифицирана функция за общ член
; недостатък: при други op може този подход да не е приложим
(define (divisors-sum n)
  (define (term i)
    (if (= (remainder n i) 0) i 0))
  (accumulate + 0 1 n term 1+))

; Зад.6 - решение с модифицирана операция
; недостатък: по- ни харесва да използваме известните операции +,*,...
(define (divisors-sum* n)
  (define (op curr rest)
    (if (= (remainder n curr) 0)
        (+ curr rest)
        rest))
  (accumulate op 0 1 n id 1+))

; Зад.6 - решение с filter
; няма недостатъци, това е най-доброто решение
(define (divisors-sum** n)
  (filter-accum + 0 1 n id 1+
                (lambda (i) (= (remainder n i) 0))))

; Зад.6 - решение с модифицирана стъпка
; недостатък: не е очевидно решение
(define (divisors-sum*** n)
  (define (divides? i) (= (remainder n i) 0)) ; за четимост
  ; идея: за даденото i проверяваме дали i+1 е делител
  (define (find-next i)
    (cond [(= i n) (+ n 1)] ; дъно на рекурсията - да върнем "невалидна" стойност
          [(divides? (+ i 1)) (+ i 1)] ; ако да, значи очевидно той е следващия делител
          [else (find-next (+ i 1))])) ; ако не, продължаваме да търсим нататък _след_ i+1
  (accumulate + 0 1 n id find-next)) ; останалото е просто

; Зад.7
(define (count p? a b)
  (accumulate + 0 a b (lambda (i) (if (p? i) 1 0)) 1+))

; Зад.8
(define (all? p? a b)
  (accumulate (lambda (x y) (and x y))
              #t
              a b
              p? 1+))

; алтернативни решения:
;(define (all? p? a b)
;  (= (count p? a b) (+ b (- a) 1)))
; ДеМорган в действие: ако за никое не е изпълнено отрицанието
; на предиката, значи предикатът е изпълнен за всички
;(define (all? p? a b)
;  (not (any? (lambda (i) (not (p? i))) a b)))

(define (any? p? a b)
  (accumulate (lambda (x y) (or x y))
              #f
              a b
              p? 1+))
; алтернативни решения:
;(define (any? p? a b)
;  (positive? (count p? a b)))
; пак ДеМорган
;(define (any? p? a b)
;  (not (all? (lambda (i) (not (p? i))) a b)))

; Зад.9 - не забравяйте, че 1 не е просто
(define (prime? n)
  (and (> n 1)
       (not (any? (lambda (i) (= (remainder n i) 0)) 2 (sqrt n)))))
; алтернативно:
;(define (prime? n)
;  (and (> n 1)
;       (zero? (count (lambda (i) (= (remainder n i) 0)) 2 (sqrt n)))))

; Зад.10
(define (constantly c)
  (lambda (x) c))

; Зад.11
(define (flip f)
  (lambda (x y) (f y x)))

; Зад.12 - полезна за all? и any?
(define (complement p?)
  (lambda (x) (not (p? x))))