#lang racket

(require racket/unsafe/ops)

(define (string->vector s)
  (build-vector (unsafe-string-length s) (lambda (i) (string-ref s i))))
  
(define (levenshtein-distance string1 string2)
    (define s1 (string->vector string1))
    (define s2 (string->vector string2))
    (define l1 (vector-length s1))
    (define l2 (vector-length s2))
    (define prev (build-vector (add1 l2) identity))
    (for ([i l1])
        (define acc (add1 i))
        (set! prev
            (build-vector (add1 l2) (lambda (j) (cond 
                [(= j 0) acc]
                [else
                    (define d1 (add1 (vector-ref prev j)))
                    (define d2 (add1 acc))
                    (define d3 (vector-ref prev (sub1 j)))
                    (if (equal? (vector-ref s1 i) (vector-ref s2 (sub1 j)))
                        null
                        (set! d3 (add1 d3)))
                    (set! acc (min d1 d2 d3))
                    acc
                ]
            )))))
    (vector-ref prev l2)
)

(provide
 (contract-out
    [levenshtein-distance (-> string? string? number?)]
 ))

(module+ test
    (require rackunit)
    (define (check-lev s1 s2 distance)
        (check-equal? (levenshtein-distance s1 s2) distance)
    )
    (check-lev "" ""    0)
    (check-lev "s" ""   1)
    (check-lev "s" "t"  1)
    (check-lev "a" "b"  1)
    (check-lev "a" "ab" 1)
    (check-lev "a" "bc" 2)
    (check-lev "fox" "fox" 0)
    (check-lev "fox" "fax" 1)
    (check-lev "folx" "fax" 2)
    (check-lev "switch" "with" 2)
    (check-lev "felix the cat" "anita the hamster" 10)
)