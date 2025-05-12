#lang racket

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