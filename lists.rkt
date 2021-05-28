#lang racket

(require racket/trace)
(require test-engine/racket-tests)

; (find-list-length '(1 2 3 4 5 6)) -> 6
(check-expect (find-list-length '(1 2 3 4 5 6)) 6)
(define (find-list-length lst)
  (if (empty? lst) 0 (+ 1 (find-list-length (cdr lst)))))
(trace find-list-length)

; (sum-list '(1 2 3)) -> 6
(check-expect (sum-list '(1 2 3)) 6)
(define (sum-list lst)
  (if (empty? lst) 0 (+ (car lst) (sum-list (cdr lst)))))
(trace sum-list)

; (tail '(1 2 3 4 5)) -> '(2 3 4 5)
(check-expect (tail '(1 2 3 4 5)) '(2 3 4 5))
(define (tail lst)
  (if (empty? lst) '() (cdr lst)))
(trace tail)

; (concat '(1 2) '(4 5)) -> '(1 2 4 5)
(check-expect (concat '(1 2) '(4 5)) '(1 2 4 5))
(define (concat fst snd)
  (append fst snd))


; (sum-all-els '(1 4(1 1 (3 9 9 9 9 9)))) -> 55
(check-expect (sum-all-els '(1 4(1 1 (3 9 9 9 9 9)))) 55)
(define (sum-all-els lst)
  (cond
    [(empty? lst) 0]
    [(number? lst) lst]
    [(list? lst) (+ (sum-all-els (car lst)) (sum-all-els (cdr lst)))]
    [else 0]))

(define (depth-aux lst max count)
  (cond
    [(empty? lst) (if (> count max) count max)]
    [(list? (car lst)) (depth-aux (car lst) max (+ count 1))]
    [else (depth-aux (cdr lst) max count)]))


; (depth '(1 2 (3 4 (5 6)))) -> 3
(check-expect (depth '(1 2 (3 4 (5 6)))) 3)
(define (depth lst)
  (cond
    [(empty? lst) 1]
    [else (depth-aux lst 0 1)]))


; (list-replace '(1 1 1 (2 2 3 5 6 1)) 1 'a) -> '(a a a (2 2 3 5 6 a))
(check-expect (list-replace '(1 1 1 (2 2 3 5 6 1)) 1 'a) '(a a a (2 2 3 5 6 a)))
(define (list-replace lst init final)
  (cond
    [(empty? lst) '()]
    [(equal? lst init) final]
    [(list? lst) (cons (list-replace (car lst) init final) (list-replace (cdr lst) init final))]
    [else lst]))

