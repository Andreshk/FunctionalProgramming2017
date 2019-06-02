#lang racket
; Март 2009
; Търсим дали л2 започва с л1
(define (begins? l1 l2)
  (cond [(null? l1) #t]
        [(null? l2) #f]
        [(not (= (car l1) (car l2))) #f]
        [else (begins? (cdr l1) (cdr l2))]))

(define (f l1 l2)
  (cond [(null? l2) 0]
        [(begins? l1 l2)
         (+ 1 (f l1 (cdr l2)))]
        [else (f l1 (cdr l2))]))

; Юли 2014
(define (totalMin fs)
  (define (helper curr lst)
    (cond [(null? lst) curr]
          [(< (curr 0) ((car lst) 0))
           (helper curr (cdr lst))]
          [else (helper (car lst) lst)]))
  (helper (car fs) (cdr fs)))

; Функцията за създаване на потоци също
; трябва да е като "специална форма"
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define (magic f i) "to-do")
(define (chainMinComps f)
  (define (helper i)
    (cons-stream (magic f i)
                 (helper (+ i 1))))
  (helper 0))

; Септември 2009
(define (countMyHead lst)
  (cond [(null? lst) 0]
        [(null? (cdr lst)) 1]
        [(= (car lst) (cadr lst))
         (+ 1 (countMyHead (cdr lst)))]
        [else 1]))

(define (compress lst)
  (let [(count (countMyHead lst))]
    (cons (if (count > 1)
              (cons (car lst) count)
              (car lst))
          (compress (drop count lst)))))

; Септември 2014
(define (perms lst)
  (if (null? lst)
      (list '())
      (apply append
         (map (lambda (x)
                (map (lambda (p) (cons x p))
                   (perms (remove x lst))))
       lst))))

; Септември 2018
;((1 2 3) (-1 0 5) (1 4 -1)) -> filter на всеки
;(() (-1 0) (-1)) -> (foldr selectlist '() ...)
;(-1 0) -> sum
;-1

(maximum lst) -> (apply max lst)
(define (selectList l1 l2)
  (if (>= (length l1) (length l2)) l1 l2))

(define (sumMaxRoots f ll)
  (apply +
    (foldr selectList '()
      (map (lambda (l)
             (filter (lambda (x) (= (f x) 0)) l)) ll))))




        
      