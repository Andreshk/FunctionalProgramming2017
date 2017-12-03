#lang racket
(define head car)
(define tail cdr)
; Зад.9.
(define (begins-with? lst1 lst2)
  (cond [(null? lst1) #t]
        [(null? lst2) #f]
        [else (and (equal? (head lst1) (head lst2))
                   (begins-with? (tail lst1) (tail lst2)))]))

(define (sublist? lst1 lst2)
  (or (begins-with? lst1 lst2)
      (sublist? lst1 (tail lst2))))

; Зад.10.
(define (remove-all val lst)
  (filter (lambda (x) (not (equal? x val))) lst))

(define (make-set lst)
  (if (null? lst)
      '()
      (cons (head lst)
            (make-set (remove-all (head lst) (tail lst))))))

; Зад.11.
(define (count val lst)
  (foldr (lambda (el result)
           (if (equal? el val)
               (+ result 1)
               result))
         0
         lst))

(define (count* val lst)
  (apply +
         (map (lambda (el) (if (equal? el val) 1 0))
                  lst)))

(define (count** val lst)
  (length (filter (lambda (el) (equal? el val)) lst)))

(define (histogram lst)
  (map (lambda (el)
         (cons el (count el lst)))
       (make-set lst)))

(define (first-row m) (head m))
(define (rest-rows m) (tail m))
(define (first-col m) (map head m))
(define (rest-cols m) (map tail m))

; (null? m) -> дъно на рекурсията, ако обхождаме по редове
; (null? (head m)) -> ако обхождаме по колони

; Зад.12.
(define (transpose m)
  (if (null? (head m))
      '()
      (cons (first-col m)
            (transpose (rest-cols m)))))

(define (transpose* m) (apply map list m))

; Зад.13.
(define (all? p? lst)
  (null? (filter (lambda (el) (not (p? el))) lst)))

(define (triangular? m)
  (or (null? m)
      (and (all? zero? (tail (first-col m)))
           (triangular? (rest-rows (rest-cols m))))))

; Зад.14.
(define (main-diag m)
  (if (null? m)
      '()
      (cons (head (first-row m)) ; (caar m)
            (main-diag (rest-cols (rest-rows m))))))

; използваме вградената list-ref: (list-ref '(a b c d) 2) -> 'c
(define (main-diag* m)
  (map (lambda (idx)
         (list-ref (list-ref m idx) idx))
       (range 0 (length m))))

; Зад.15.
; обръщаме всички редове и на полученото взимаме главния диагонал
(define (2nd-diag m)
  (main-diag (map reverse m)))

; Зад.16.
(define (descartes lst1 lst2)
  (define (f x)
    (map (lambda (el) (cons el x)) lst1))
  (apply append (map f lst2)))

; можем да индексираме елементите от 0 до n*m-1
; и всеки индекс да съпоставим двойка индекси в първия и втория списък
(define (descartes* lst1 lst2)
  (define len1 (length lst1))
  (define len2 (length lst2))
  (map (lambda (idx) (cons (list-ref lst1 (remainder idx len1))
                           (list-ref lst2 (quotient idx len1))))
       (range 0 (* len1 len2))))



