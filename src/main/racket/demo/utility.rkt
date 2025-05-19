#lang racket

(define (iterate-to-fixed start-value fn)
    (define iterate
       (fn start-value)
    )
    (cond 
        ((equal? start-value iterate) start-value)
        (else (iterate-to-fixed iterate fn))
    )
)

(module+ test
  (require rackunit)
  (define (conditional-inc x)
    (if (< x 10) (add1 x) x)
  )
  (check-equal? (iterate-to-fixed 3 conditional-inc) 10)
  (check-equal? (iterate-to-fixed 70 conditional-inc) 70)
)



