#lang racket

(provide
 (contract-out
  ;; calculate the collatz iteration of a number
  [collatz (-> number? number?)]
  )
 (contract-out
  ;; calculate the collatz degree of a number
  [degree (-> number? number?)]
  )
)

(define (collatz n)
  (if (= 0 (remainder n 2))
      (quotient n 2)
      (add1 (* 3 n))
  )
)

(define (degree n)
  (if (= n 1)
      0
      (add1 (degree (collatz n)))
      )
  )

(module+ test
  (require rackunit)
  (check-equal? (collatz 3) 10)
  (check-equal? (collatz 8) 4)
  (check-equal? (degree 8) 3)
  (check-equal? (degree 17) 12)
)



