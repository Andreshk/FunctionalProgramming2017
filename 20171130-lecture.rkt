; iei
(define (fact n)
  (define (helper i res)
    (if (> i n)
        res
        (helper (+ i 1) (* i res))))
  (helper 1 1))

(define bigpromise (even? (fact 30000)))
; Още нямаме дефинирано а - но то ще ни трябва чак
; когато force-нем обещанието и се оцени израза (+ а 3)
(define undefined (delay (+ a 3)))

; проблем 1: не кешира резултата -> реално това е "бонус" от езика
; проблем 2: оценява expr - а не трябва
(define (delay* expr)
  (lambda () expr))
(define (force* promise)
  (promise))

(define the-empty-stream '())
; същият проблем: оценява t, а не трябва
;(define (cons-stream h t)
;  (cons h (delay t)))
; Дефиниция на cons-stream като специална форма
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))
; Стандартни методи за достъп, аналогични на списъците
(define head car)
(define (tail s) (force (cdr s)))
(define empty-stream? null?)

; Когато създадем списък така...
(define s (cons-stream 1
            (cons-stream 2
              (cons-stream 3
                the-empty-stream))))
; ...кодът, който се *генерира* и реално оценява, е:
(define s* (cons 1
             (delay (cons 2
                      (delay (cons 3
                               (delay '())))))))

(define s2 (cons-stream 1
             (cons-stream b
               (cons-stream 3
                 the-empty-stream))))

; Специални форми, които сме срещали досега
; if, cond, define, and, or, quote
; lambda, let, let*, letrec, delay

; Ще я използваме най-вече за да проверяваме
; дали сме конструирали правилно потоците си
(define (take n s)
  (if (or (empty-stream? s)
          (= n 0))
      '()
      (cons (head s)
            (take (- n 1) (tail s)))))

(define (from n) ; генератор
  (cons-stream n (from (+ n 1))))
(define nats (from 0))

(define (generate-fibs a b) ; генератор
  (cons-stream a (generate-fibs b (+ a b))))
(define fibs (generate-fibs 0 1))

(define (zip-streams op s1 s2)
  (cons-stream (op (head s1) (head s2))
               (zip-streams op (tail s1) (tail s2))))

; директна рекурсия - не е проблем (аналогично на undefined по-горе)
(define ones (cons-stream 1 ones))
(define nats (cons-stream 0 (zip-streams + nats ones)))

(define triangs
  (cons-stream 1
    (zip-streams + (from 2) triangs)))

(define (tr-gen i) ; може и с генератор
  (cons-stream (/ (* i (+ i 1)) 2)
               (tr-gen (+ i 1))))
(define triangs* (tr-gen 1))

; малък, прост пример
(define squares (zip-streams * nats nats))

(define (filter-stream p? str)
  (if (p? (head str))
      (cons-stream (head str)
                   (filter-stream p? (tail str)))
      (filter-stream p? (tail str))))

(define (notdivides d)
  (lambda (n) (> (remainder n d) 0)))
; сито на Ератостен
(define (sieve stream)
  (cons-stream (head stream) ; взимаме първото число
               (sieve (filter-stream ; и после пресищаме всички числа, които не се делят на него
                       (notdivides (head stream))
                       (tail stream)))))

(define primes (sieve (from 2)))