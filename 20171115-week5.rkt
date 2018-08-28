#lang racket
; За четимост на кода
(define head car)
(define tail cdr)

; Зад.0 - кръстени са със звездички, защото има такива вградени
(define (length* lst)
  (foldr (lambda (el result) (+ 1 result)) 0 lst))

(define (reverse* lst)
  (foldr (lambda (el result) (append result (list el))) '() lst))

(define (map* f lst)
  (foldr (lambda (el result) (cons (f el) result)) '() lst))

(define (filter* p? lst)
  (foldr (lambda (el result) (if (p? el)
                                 (cons el result)
                                 result))
         '() lst))

; Зад.1
(define (nth n lst)
  (cond [(null? lst) #f]
        [(= n 0) (head lst)]
        [else (nth (- n 1) (tail lst))]))

; Зад.2
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (id x) x)
(define (1+ x) (+ x 1))
(define (range* from to)
  (accumulate cons '() from to id 1+))

; Зад.3
(define (digit-list n)
  (define (helper n)
    (if (< n 10)
        (list n)
        (cons (remainder n 10)
              (helper (quotient n 10)))))
  (reverse (helper n)))

(define (digit-list* n)
  (define (helper n res)
    (if (< n 10)
        (cons n res)
        (helper (quotient n 10)
                (cons (remainder n 10) res))))
  (helper n '()))

; Зад.4
(define (take n lst)
  (cond [(null? lst) '()]
        [(= n 0) '()]
        [else (cons (head lst) (take (- n 1) (tail lst)))]))

(define (drop n lst)
  (cond [(null? lst) '()]
        [(= n 0) lst]
        [else (drop (- n 1) (tail lst))]))

; Зад.5
;(define (all? p? lst)
;  (cond [(null? lst) #t]
;        [(not (p? (head lst))) #f]
;        [else (all? p? (tail lst))]))

(define (all? p? lst)
  (foldr (lambda (el result) (and (p? el) result))
         #t
         lst))

(define (any? p? lst)
  (not (all? (lambda (x) (not (p? x))) lst))) ; стандартно

; Зад.6 & 7
(define (zipWith f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (head lst1) (head lst2))
            (zipWith f (tail lst1) (tail lst2)))))

(define (zip lst1 lst2)
  (zipWith cons lst1 lst2))

; Зад.8
(define (sorted? lst)
  (if (or (null? lst) (null? (tail lst)))
      #t
      (and (<= (head lst) (head (tail lst)))
           (sorted? (tail lst)))))

(define (sorted?* lst)
  (define (make-pairs lst) (zip lst (tail lst)))
  (all? (lambda (p) (<= (car p) (cdr p)))
        (make-pairs lst)))

; Зад.9
(define (uniques lst)
  (define (remove-my-head lst) (remove (head lst) (tail lst)))
  (if (null? lst)
      '()
      (cons (head lst)
            (uniques (remove-my-head lst)))))

(define (uniques* lst)
  (define (helper lst res)
    (cond [(null? lst) res]
          [(member (head lst) res) (helper (tail lst) res)]
          [else                    (helper (tail lst) (cons (head lst) res))]))
  (helper lst '()))

; foldr за закуска, обед и вечер
(define (uniques** lst)
  (foldr (lambda (el result)
           (if (member el result)
               result
               (cons el result)))
         '() lst))

; Зад.10
(define (extract-ints lst) (filter integer? lst))

; Зад.11
(define (insert val lst)
  (cond [(null? lst) (list val)]
        [(< (head lst) val) (cons (head lst) (insert val (tail lst)))]
        [else (cons val lst)]))

; Зад.12
(define (insertion-sort lst)
  (foldr insert '() lst)) ; simple

; Зад.13
(define (my-arity . xs) (length xs))

; Зад.14
(define (compose* f g) (lambda (x) (f (g x))))
(define (compose . fns)
  (foldr compose* id fns))

; Зад.15
(define (group-by f lst)
  (define (filter-by-value val)
    (filter (lambda (x) (equal? (f x) val)) lst))
  (let* [(all-values (map f lst))
         (unique-values (uniques all-values))]
    (map (lambda (v) (cons v (list (filter-by-value v))))
         unique-values)))

; Зад.16
(define (zipWith* f . lsts)
  (define (heads lsts) (map head lsts))
  (define (tails lsts) (map tail lsts))
  (cond [(null? lsts) '()]
        [(any? null? lsts) '()]
        [else (cons (apply f (heads lsts))
                    (apply zipWith* (cons f (tails lsts))))]))

;(define zipWith* map) ; surprise-surprise
