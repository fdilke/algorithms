#lang racket

(require rackunit)

(define (fibo n)
  (if (< n 2)
        n
      (+ (fibo(sub1 n)) (fibo (- n 2)))
  )
)

;; find all pythagorean triples below a given number
(define (pyth-triples n)
     (for*/list (
        [i (in-range 1 n)] 
        [j (in-range i n)] 
        [k (in-range j n)] 
        #:when (= (sqr k) (+ (sqr i) (sqr j)))
        ) 
        (list i j k)
    )
)

(struct document (author title content))

(define pyth-triples-tests
    (test-suite "Test pyth triples code"
        #:before (lambda () (println "Before triples"))
        #:after  (lambda () (println "After triples"))
        (test-case "Finds triples"
            (check-equal? (pyth-triples 10) '((3 4 5)) "Not equal :(")
        )
    )
)

(define fibo-structs-tests
    (test-suite "Test fibo and structs"
        #:before (lambda () (println "Before fibo"))
        #:after  (lambda () (println "After fibo"))
        pyth-triples-tests
        (test-case
            "Fibo behaves"
                (check-equal? (fibo 5) 5 "It's not!")
                (check-pred (λ (x) (= x 8)) (fibo 6) "It's not!")
        )    
        (test-case "Noddy language features"
            (check-equal?
                (stream->list (in-range 5))
                '(0 1 2 3 4)
            )
            (check-pred symbol? 'a_symbol)
            (check-false (symbol? "piggin"))
            (check-equal? [+ 3 4] 7)
            (check-equal? {- 3 4} -1)
            (check-equal?
                (for/list ([i '(1 2 3)] [j '(4 5 6)]) (* i j))
                '(4 10 18)
            )
            (check-equal?
                (for*/list ([i '(1 2 3)] [j '(4 5 6)]) (* i j))
                '(4 5 6 8 10 12 12 15 18)
            )
        )
        (test-case "Documents work"
            (check-equal? (document-author
                    (document "Herman Melville" "Moby Dick" "Call me Ishmael")
                ) "Herman Melville"
            )
            (check-equal? 
                (document-title
                    (document "Enid Blyton" "Noddy in Toyland" "Beep beep")
                ) 
                "Noddy in Toyland"
            )
            (check-pred document? 
                (document "James Lee Burke" "Heartwood" "southern drama")
            )
            (check-false
                (document? 0)
            )
        )
        (test-suite "We can nest test suites"
            (test-case "Int comparison is sane"
                (check < 1 2)
            )
        )
        (test-suite "a suite to test continuations"
            (test-case "saving a context"
                (define saved-k #f)
                (define (save-comp!)
                    (call-with-composable-continuation
                    (lambda (k) ; k is the captured continuation
                    (set! saved-k k)
                    0)))
                (check = 3 (+ 1 (+ 1 (+ 1 (save-comp!)))))
            )
        )
        (test-case "Int comparison is sane"
            (check < 1 2)
        )
    )
)

(require rackunit/text-ui)
(run-tests fibo-structs-tests 'verbose)
;; (require rackunit/gui)
;; (test/gui fibo-structs-tests)
