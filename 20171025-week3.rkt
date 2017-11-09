#lang racket

;(define (fib n)
;  (if (< n 2)
;      n
;      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fib n)
  (define (helper a b i)
    (if (= i n)
        b
        (helper b (+ a b) (+ i 1))))
  (if (= n 0)
      0
      (helper 0 1 1)))

(define (fact n)
  (define (helper res i)
    (if (> i n)
        res
        (helper (* res i) (+ i 1))))
  (helper 1 1))

(define (last n) (remainder n 10))
(define (digits n)
  (if (< n 10)
      1
      (+ 1 (digits (quotient n 10)))))

(define (digits* n)
  (define (helper n res)
    (if (< n 10)
        (+ res 1)
        (helper (quotient n 10) (+ res 1))))
  (helper n 0))

; работи само за неотрицателни цели числа
(define (reverse-int n)
  (if (< n 10)
      n
      (let [(tmp (reverse-int (quotient n 10)))]
        (+ tmp
           (* (last n) (expt 10
                             (digits tmp)))))))

(define (reverse-int* n)
  (define (helper n res)
    (if (= n 0)
        res
        (helper (quotient n 10) (+ (* res 10) (last n)))))
  (if (< n 0)
      (- (reverse-int* (- n)))
      (helper n 0)))

(define (palindrome? n)
  (= n (reverse-int* n)))
  
(define (perfect? n)
  (= (divisors-sum n) (* 2 n)))

(define (divisors-sum n)
  (define (helper i res)
    (cond [(> i n) res]
          [(= (remainder n i) 0) (helper (+ i 1) (+ res i))]
          [else                  (helper (+ i 1) res)]))
  (helper 1 0))










