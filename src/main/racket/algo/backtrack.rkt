#lang racket

(require data/either)

(define (backtrack start-node explore)
    (define (result input-list unprocessed)
        (stream-lazy
            (cond
                ((stream-empty? unprocessed)
                    (if (null? input-list)
                        (stream)
                        (result (rest input-list)
                            (stream-append unprocessed (explore (first input-list)))
                        )
                    )
                )
                (else                    
                    (let (
                        (an-either (stream-first unprocessed))
                        (remaining (stream-rest unprocessed))
                        )
                        (either 
                            (λ (node)
                                (result (cons node input-list) remaining)
                            )
                            (λ (solution)
                                (stream-cons solution 
                                    (result input-list remaining))
                            )
                            an-either
                        )
                    )
                )
            )
        )
    )
    (result (list start-node) (stream))
)

(provide
 (contract-out
    [backtrack (parametric->/c [A] (-> A (-> A stream?) stream?))]
 ))

(module+ test
  (require rackunit)
  (check-equal? ; a non starter node that can't be explored
    (stream->list
        (backtrack 0 (lambda (x) (stream))))
    null
  )
  (check-equal? ; a one-off node that explores to one level
    (stream->list
        (backtrack 0 (lambda (x) (stream (success 1)))))
    '(1)
  )
  (check-equal? ; a tree that increments a value to 5
    (stream->list
        (backtrack 0 (lambda (i)
            (stream
                (if (= i 5)
                    (success #t)
                    (failure (add1 i))
                )
        ))))
    '(#t)
  )
  (check-equal? ; find all bit-sequences of length 3
    (stream->list
        (backtrack '() (lambda (prefix)
            (if (= (length prefix) 3)
                (stream (success prefix))
                (stream
                    (failure (cons #t prefix))
                    (failure (cons #f prefix))
                )))))
     '(
        (#f #f #f) (#t #f #f)
        (#f #t #f) (#t #t #f)
        (#f #f #t) (#t #f #t)
        (#f #t #t) (#t #t #t)
    ))
)