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

(provide
 (contract-out
  ;; iterate a function on a start value until it's fixed
    [iterate-to-fixed (parametric->/c [A] (-> A (-> A A) A))]
  )
)
  

(module+ test
  (require rackunit)
  (define (conditional-inc x)
    (if (< x 10) (add1 x) x)
  )
  (check-equal? (iterate-to-fixed 3 conditional-inc) 10)
  (check-equal? (iterate-to-fixed 70 conditional-inc) 70)
  (define (raw-sqrt n) (/ (+ n (/ 120 n)) 2.0))
  (check-equal? (iterate-to-fixed 3 raw-sqrt) 10.954451150103322)
)



