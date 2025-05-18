#lang racket
 
(provide
 (contract-out
  ;; convert a fahrenheit temperature to a celsius
  [fahrenheit->celsius (-> number? number?)]))
 
(define (fahrenheit->celsius f)
  (/ (* 5 (- f 32)) 9))
 
(module+ test
  (require rackunit)
  (check-equal? (fahrenheit->celsius -40) -40)
  (check-equal? (fahrenheit->celsius 32) 0)
  (check-equal? (fahrenheit->celsius 212) 100))
