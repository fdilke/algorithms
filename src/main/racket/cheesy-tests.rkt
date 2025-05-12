#lang racket

(require rackunit)
(require rackunit/text-ui)

(define (fibo n)
  (if (< n 2)
        n
      (+ (fibo(sub1 n)) (fibo (- n 2)))
  )
)

(struct document (author title content))




(define fibo-structs-tests
    (test-suite "Test fibo and structs"
        #:before (lambda () (display "Before"))
        #:after  (lambda () (display "After"))
        (test-case
            "Fibo behaves"
                (check-equal? (fibo 5) 5 "It's not!")
                (check-pred (λ (x) (= x 8)) (fibo 6) "It's not!")
        )    
        (test-case "Documents work"
            (check-equal? (document-author
                    (document "Herman Melville" "Moby Dick" "Call me Ishmael")
                ) "Herman Melville"
            )
            (check-equal? (document-title
                    (document "Enid Blyton" "Noddy in Toyland" "Beep beep")
                ) "Noddy in Toyland"
            )
        )
        (test-suite "We can nest test suites"
            (test-case "Int comparison is sane"
                (check < 1 2)
            )
        )
    )
)

(run-tests fibo-structs-tests 'verbose)
