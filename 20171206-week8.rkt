#lang racket
(define G '((a b c d) ; от а има ребра към b,c,d
            (b e f d) ; може да бъде и ориентиран
            (c a d)
            (d b c g)
            (e)       ; връх без наследници
            (f b e)
            (g a)))
          
(define G1 '((a b)
             (b a)
             (c)
             (d c)))

(define head car)
(define tail cdr)
; Зад.1.
(define (vertices g)
  (map head g))
(define (successors v g)
  (or (assoc v g) '()))
(define (has-edge? u v g)
  (member v (successors u g)))

; Зад.2.
(define (add-vertex v g)
  (if (member v (vertices g))
      g
      (cons (list v) g)))

(define (add-edge u v g)
  (cond [(has-edge? u v g) g]
        [(and (member u (vertices g))
              (member v (vertices g)))
           (map (lambda (lst) (if (equal? u (head lst))
                                  (append lst (list v))
                                  lst))
                g)]
        [else (add-edge u v (add-vertex u (add-vertex v g)))]))

(define (foldr op nv lst)
  (if (null? lst)
      nv
      (op (head lst)
          (foldr op nv (tail lst)))))

; за удобство
(define (make-from-edges lst)
  (foldr (lambda (e res) (add-edge (car e) (cdr e) res))
         '()
         lst))

; Зад.3.
;(define (contains-path? path g)
;  (cond [(or (null? path)
;             (null? (tail path))) #t]
;        [(has-edge? (car path) (cadr path) g)
;           (contains-path? (tail path) g)]
;        [else #f]))
      
(define (contains-path? path g)
  (or (null? path)
      (null? (tail path))
      (and (has-edge? (car path) (cadr path) g)
           (contains-path? (tail path) g))))
         
;(define (contains-path? path g)
;  (all? (lambda (p) (has-edge? (car p) (cdr p) g))
;        (zip path (tail path))))

; Зад.4.
(define (predecessors v g)
  (filter (lambda (u) (has-edge? u v g))
          (vertices g)))

; Зад.5.
(define (extend-path path g)
  (let* [(last-v (head (reverse path)))
         (succs (successors last-v g))
         (filtered (filter (lambda (v) (not (member v path)))
                           succs))]
    (map (lambda (v) (append path (list v)))
         filtered)))

; Зад.6.
(define (all-paths g)
  (define n (length (vertices g)))
  (define (helper i result currents)
    (if (or (> i n)
            (null? currents))
        result
        (helper (+ i 1)
                (append currents result)
                (apply append
                       (map (lambda (path) (extend-path path g)) currents)))))
    (helper 1 '() (map list (vertices g))) )

; бонус - намиране на всички прости цикли, без премахване на ротациите
(define (last lst) (head (reverse lst)))
(define (all-cycles g)
  (filter (lambda (path)
            (and (has-edge? (last path)
                       (head path)
                       g)
                 (> (length path) 1)))
          (all-paths g)))

; Зад.7.
(define (edge-list g)
  (define (make-pairs-single l) (map (lambda (v) (cons (car l) v))
                                     (cdr l)))
  (apply append (map make-pairs-single g)))

; Зад.8. - обръщане на всички ребра в граф
(define (invert g)
  (define (flip p) (cons (cdr p) (car p)))
  (make-from-edges (map flip (edge-list g))))

; Зад.9.
(define (bfs v g)
  (define (get-next-level current visited)
    (apply append (map (lambda (v) (filter (lambda (v) (not (member? v visited)))
                                   (successors v g)))
                       current)))
  (define (helper current result)
    (let [(next (get-next-level current result))]
      (if (null? next)
          result
          (helper next (append result next)))))
  (helper (list v) (list v)))

(define test '((a b c)
               (b d)
               (c d)
               (d)))
        
; Задачи за потоци  
;(define-syntax cons-stream
;  (syntax-rules ()
;    ((cons-stream h t) (cons h (delay t)))))
;(define (head str) (car str))
;(define (tail str) (force (cdr str)))

;(define (append-to-stream lst str)
;  (if (null? lst)
;      str
;      (cons-stream (head lst)
;                   (append-to-stream (tail lst)
;                                     str))))
                                   
;(define head* head)
;(define (tail* str) (force (tail str)))
;(define (take n str)
;  (if (= n 0)
;      '()
;      (cons (head* str)
;            (take (- n 1) (tail* str))))))
;          
;(define (append-to-stream lst str)
;  (foldr cons-stream str lst)
;(define (cycle lst)
;  (append-to-stream lst (cycle lst)))

;(define ones (cycle '(1)))