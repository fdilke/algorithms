#lang racket

; the 'stuffed dog problem' inspired by a Mittens dalle:
; given 14 (or more generally, n) stuffed dogs, how many ways can you arrange them in an L-shaped
; pattern ; more precisely, as n = ab - cd where a, b, c, d are positive integers 
; with a > c > 0, b > d > 0, a >= b.
; We have the bound n = ab - cd >= ab - (a-1)(b-1) = a + b - 1 >= a + 1.

(define (foofy n)
    (let (
        (a-max (sub1 n))
    )
    (apply append (for*/list (
        (a (in-range a-max 1 -1))
        (b (in-range a 1 -1))
        (c (in-range (sub1 a) 0 -1))
        (d (in-range (sub1 b) 0 -1))
        )
        (if (and (or (> a b) (>= c d)) (= n (- (* a b) (* c d))))
            (list (list (list a b) (list c d)))
            '())
))))

(provide
 (contract-out
;    [foofy (-> number? any)]
    [foofy (-> number? (listof (listof (listof number?))))]
  ))

(module+ test
  (require rackunit)
  (check-equal? (foofy 1) '())
  (check-equal? (foofy 3) '(((2 2)(1 1))))
  (check-equal? (foofy 4) '(((3 2)(2 1))))
)



