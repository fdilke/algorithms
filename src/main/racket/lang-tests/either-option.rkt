#lang racket

(require data/either)
(require data/maybe)
(require data/applicative)
(require data/functor)

(module+ test
  (require rackunit)
  (check-equal?
    (map add1 (just 3))
    (just 4)
  )
  (check-equal?
    (map add1 nothing)
    nothing
  )
  (check-equal?
    (map add1 (success 1))
    (success 2)
  )
  (check-equal?
    (map add1 (failure 1))
    (failure 1) ; only has effect on a successful either
  )
  (check-equal?
    ((pure +) (success 1) (success 2))
    (success 3)
  )
  (check-equal?
    (either add1 sub1 (failure 3))
    4
  )
  (check-equal?
    (either add1 sub1 (success 3))
    2
  )
)