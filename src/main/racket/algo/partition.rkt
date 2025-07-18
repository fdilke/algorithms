#lang racket

(require "cache.rkt")

(define partition-count
    (linear-cache (lambda (n)
    (if (= n 0)    
        1
        (let loop (
                (k 1)
                (pent1 1)
                (pent2 2)
                (signPositive #t)
                (sum 0)
            )
            (if (>= n pent1) (let
                ([subsum (partition-count (- n pent1))])
                (if (>= n pent2)
                    (set! subsum (+ subsum (partition-count (- n pent2))))
                    #t
                )
                (loop
                    (add1 k)
                    (+ pent2 k k 1)
                    (+ pent2 k k k 2)
                    (not signPositive)
                    (if signPositive
                        (+ sum subsum)
                        (- sum subsum)
                    )
                )
            )
            sum
        ))))))

(provide
 (contract-out
    [partition-count (-> number? number?)]
 ))

(module+ test
  (require rackunit)
  (check-equal? 
    (map partition-count (range 11))
    '(1 1 2 3 5 7 11 15 22 30 42)
  )
)

