#lang racket

(require "collatz.rkt")

(define (collatz-highs)
  (define highest 0)
  (for* ([i (in-naturals 1)])
    (define value (degree i))
    ;; (println (string-append "trying:" (~v i) " -> " (~v value)))
    (cond
      ( (> value highest) (set! highest value) (println (string-append (~v i) "] " (~v value))))
    )
  )
)

(collatz-highs)
