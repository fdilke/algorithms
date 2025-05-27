#lang racket

(struct document ([author #:mutable] title content))

(struct fruit (name [weight-in-grams #:mutable]))
(struct special-fruit fruit (special-name))

(module+ test
  (require rackunit)
  (define test-doc
    (document "Colton Whitehead" "Nickel Boys" "twisty memoir")
  )
  (check-equal? (document-author test-doc) "Colton Whitehead")
  (set-document-author! test-doc "Colson Whitehead")
  (check-equal? (document-author test-doc) "Colson Whitehead")
  (define a-cox-pippin 
    (special-fruit "apple" 100 "Cox Pippin")
  )
  (define a-cox-pippin-2 
    (special-fruit "apple" 200 "Cox Pippin Variant")
  )
  (check-equal? (special-fruit-special-name a-cox-pippin) "Cox Pippin")
  (check-equal? (special-fruit-special-name a-cox-pippin-2) "Cox Pippin Variant")
  (check-equal? (fruit-name a-cox-pippin) "apple")
  (check-equal? (fruit-name a-cox-pippin-2) "apple")
  (check-equal? (fruit-weight-in-grams a-cox-pippin) 100)
  (check-equal? (fruit-weight-in-grams a-cox-pippin-2) 200)
)
