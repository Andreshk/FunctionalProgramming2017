#lang racket
(define head car)
(define tail cdr)

; Зад.1
; Функция, "абстрахираща" дали
; търсим минимум или максимум
(define (magic op f lst)
  (foldr (lambda (x y)
           (if (op (f x) (f y)) x y))
         (head lst)
         (tail lst))) ; можем да "обходим" само опашката
                      ; взимайки главата за null value

(define (argmin f lst)
  (if (null? lst) #f
      (magic <= f lst)))

(define (argmax f lst)
  (if (null? lst) #f
      (magic >= f lst)))

; Зад.2
(define (number->list n)
  (if (< n 10) (list n)
      (append (number->list (quotient n 10))
              (list (remainder n 10)))))

(define (list->number lst)
; (foldl (lambda (d num) (+ (* 10 num) d)) 0 lst))
  (define (helper lst res)
    (if (null? lst) res
        (helper (tail lst) (+ (* 10 res) (head lst)))))
  (helper lst 0))

(define (reduce n)
  (if (< n 10) n
      (let* [(digits (number->list n))
             (max-digit (apply max digits))
             (new-num (list->number (remove max-digit digits)))]
        (reduce (* max-digit
                   new-num)))))

; Зад.3
; Всяка функция разчита на итеративна помощна
; функция (все пак са _итеративни_ числени методи)
(define (approx-zero? x)
  (< (abs x) 0.00000001))

(define (find-root-binary f a b eps)
  (define (helper ai bi count)
    (let* [(mid (/ (+ ai bi) 2))
           (fmid (f mid))]
      (cond [(or (< (- bi ai) eps)
                 (approx-zero? fmid)) (cons (exact->inexact mid) count)]
            [(< (* (f ai) fmid) 0) (helper ai mid (+ count 1))]
            [else (helper mid bi (+ count 1))])))
  (helper a b 0))

(define (find-root-secant f a b eps)
  (define (helper xi-1 xi count) ; за разлика от други езици, тук "xi-1" е съвсем валидно име
    (if (< (abs (- xi xi-1)) eps)
        (cons (exact->inexact xi) count)
        (helper xi (- xi (/ (* (f xi) (- xi xi-1)) (- (f xi) (f xi-1)))) (+ count 1))))
  (helper a b 0))

(define (derive f)
  (define dx 0.000001)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))

(define (find-root-newton f a b eps)
  (define (helper xi-1 xi count)
    (if (< (abs (- xi xi-1)) eps)
        (cons (exact->inexact xi) count)
        (helper xi (- xi (/ (f xi) ((derive f) xi))) (+ count 1))))
  (helper a b 0))

; За удобство връщаме и по един символ за "надписване"
(define (compare-methods f a b eps)
  (list (cons 'binary (cdr (find-root-binary f a b eps)))
        (cons 'secant (cdr (find-root-secant f a b eps)))
        (cons 'newton (cdr (find-root-newton f a b eps)))))