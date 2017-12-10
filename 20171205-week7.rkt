#lang racket
(define head car)
(define tail cdr)
(define (first-col m) (map head m))
(define (rest-cols m) (map tail m))
(define (first-row m) (head m))
(define (rest-rows m) (tail m))

; зад. от миналогодишно контролно
(define (any? p? lst)
  (not (null? (filter p? lst))))
(define (all? p? lst)
  (null? (filter (lambda (x) (not (p? x))) lst)))

(define (subset? lst1 lst2)
  (all? (lambda (x) (member x lst2)) lst1))

(define (all-in-some-row c m)
  (any? (lambda (row) (subset? c row)) m))

(define (find-columns m)
  (define (helper currm res)
    (cond [(null? (head currm)) res]
          [(all-in-some-row (first-col currm) m)
             (helper (rest-cols currm) (+ res 1))]
          [else
             (helper (rest-cols currm) res)]))
  (helper m 0))

(define (transpose m) (apply map list m))
(define (find-columns* m)
  (length
   (filter (lambda (col)
             (any? (lambda (row)
                     (subset? col row)) m))
           (transpose m))))

; общ "интерфейс" за дървета
(define empty-tree '())
(define (make-tree root left right) (list root left right))
(define (make-leaf root) (make-tree root empty-tree empty-tree))
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)

; тестовия субект за задачите ни
(define t
  (make-tree 1
             (make-tree 2 (make-leaf 3) empty-tree)
             (make-tree 4
                        (make-leaf -5)
                        (make-tree 8
                                   (make-tree 6
                                              empty-tree
                                              (make-leaf 7))
                                   empty-tree))))

; Зад.1.
(define (tree-sum t)
  (if (empty-tree? t)
      0
      (+ (root-tree t)
         (tree-sum (left-tree t))
         (tree-sum (right-tree t)))))

; Зад.2.
(define (tree-max t)
  (if (empty-tree? t)
      -inf.0
      (max (root-tree t)
           (tree-max (left-tree t))
           (tree-max (right-tree t)))))

; Зад.3.
(define (tree-level k t)
  (cond [(empty-tree? t) '()]
        [(= k 0) (list (root-tree t))]
        [else (append (tree-level (- k 1) (left-tree t))
                      (tree-level (- k 1) (right-tree t)))]))

; Зад.4.
(define (height t)
  (if (empty-tree? t)
      0
      (+ 1 (max (height (left-tree t))
                (height (right-tree t))))))

(define (all-levels t)
  (map (lambda (k) (tree-level k t))
       (range 0 (height t))))

; Зад.5.
(define (tree-map f t)
  (if (empty-tree? t)
      t
      (make-tree (f (root-tree t))
                 (tree-map f (left-tree t))
                 (tree-map f (right-tree t)))))

; Зад.6.
(define (tree->list t)
  (if (empty-tree? t)
      '()
      (append (tree->list (left-tree t))
              (list (root-tree t))
              (tree->list (right-tree t)))))

; Зад.7.
(define (bst-insert val t)
  (cond [(empty-tree? t) (make-leaf val)]
        [(< val (root-tree t))
           (make-tree (root-tree t)
                      (bst-insert val (left-tree t))
                      (right-tree t))]
        [else ; решаваме, че не е проблем да съдържаме дубликати
           (make-tree (root-tree t)
                      (left-tree t)
                      (bst-insert val (right-tree t)))]))

; за удобство
(define (list->bst lst)
  (foldr bst-insert empty-tree lst))

; Зад.8.
(define (tree-sort lst)
  (tree->list (list->bst lst)))
;(define tree-sort (compose tree->list list->bst))

; най-бързата сортировка за писане
(define (quick-sort lst)
  (if (or (null? lst) (null? (tail lst)))
      lst
      (append (quick-sort (filter (lambda (x) (< x (head lst))) lst))
                          (filter (lambda (x) (= x (head lst))) lst)
              (quick-sort (filter (lambda (x) (> x (head lst))) lst)))))

; Зад.9.
(define (valid-bst? t)
  (define (helper t a b)
    (or (empty-tree? t)
        (let [(r (root-tree t))]
          (and (<= a r)
               (>= b r)
               (helper (left-tree t) a r)
               (helper (right-tree t) r b)))))
  (helper t -inf.0 +inf.0))


