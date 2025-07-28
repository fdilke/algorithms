#lang racket

; inspired by an IMO question from Fin:
; if in the sequence a_1, a_2, ... each entry has at least 3 proper divisors and
; for i > 1, a_i is the sum of the 3 largest proper divisors of a_{i -1}, what can a_1 be?

(require data/maybe)

(define (proper-divisors n)
    (define (is-pd d)
        (and )
        (equal? 0 (remainder n d))
    )
    (filter is-pd (range 1 n))
)

(define (coll-var n)
    (let (
        [divs (proper-divisors n)]
    )
    (if (< (length divs) 3) 
        nothing
        (just (apply + (take (reverse divs) 3)))
)))

(define (is-possible a1)
    (let loop (
        [a a1]
        [so-far (list a1)])
            (maybe #f (lambda (sumdivs) 
                (if (member sumdivs so-far) 
                    #t
                    (loop sumdivs (cons sumdivs so-far))
                ))
                (coll-var a)
)))

(provide
 (contract-out
    [proper-divisors (-> number? (listof number?))]
    [coll-var (-> number? (maybe/c number?))]
    [is-possible (-> number? boolean?)]
  ))

(module+ test
  (require rackunit)
  (check-equal? (proper-divisors 3) '(1))
  (check-equal? (proper-divisors 8) '(1 2 4))
  (check-equal? (proper-divisors 6) '(1 2 3))
  (check-equal? (proper-divisors 72) '(1 2 3 4 6 8 9 12 18 24 36))
  (check-equal? (coll-var 3) nothing)
  (check-equal? (coll-var 9) nothing)
  (check-equal? (coll-var 6) (just 6))
)
